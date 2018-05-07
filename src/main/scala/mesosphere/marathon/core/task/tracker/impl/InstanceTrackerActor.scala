package mesosphere.marathon
package core.task.tracker.impl

import akka.Done
import akka.actor.SupervisorStrategy.Escalate
import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.pipe
import com.typesafe.scalalogging.StrictLogging
import mesosphere.marathon.core.appinfo.TaskCounts
import mesosphere.marathon.core.instance.Instance
import mesosphere.marathon.core.instance.update.{ InstanceChange, InstanceDeleted, InstanceUpdateEffect, InstanceUpdateOperation, InstanceUpdated }
import mesosphere.marathon.core.task.tracker.impl.InstanceTrackerActor.{ Ack, ForwardTaskOp, RepositoryStateUpdated }
import mesosphere.marathon.core.task.tracker.{ InstanceTracker, InstanceTrackerUpdateStepProcessor }
import mesosphere.marathon.metrics.AtomicGauge
import mesosphere.marathon.state.{ PathId, Timestamp }
import mesosphere.marathon.storage.repository.InstanceRepository

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.control.NonFatal

object InstanceTrackerActor {
  def props(
    metrics: ActorMetrics,
    taskLoader: InstancesLoader,
    updateStepProcessor: InstanceTrackerUpdateStepProcessor,
    taskUpdaterProps: ActorRef => Props,
    repository: InstanceRepository): Props = {
    Props(new InstanceTrackerActor(metrics, taskLoader, updateStepProcessor, taskUpdaterProps, repository))
  }

  /** Query the current [[InstanceTracker.SpecInstances]] from the [[InstanceTrackerActor]]. */
  private[impl] case object List

  private[impl] case class Get(instanceId: Instance.Id)

  /** Forward an update operation to the child [[InstanceUpdateActor]]. */
  private[impl] case class ForwardTaskOp(deadline: Timestamp, instanceId: Instance.Id, op: InstanceUpdateOperation)

  /** Describes where and what to send after an update event has been processed by the [[InstanceTrackerActor]]. */
  private[impl] case class Ack(initiator: ActorRef, effect: InstanceUpdateEffect) extends StrictLogging {
    def sendAck(): Unit = {
      val msg = effect match {
        case InstanceUpdateEffect.Failure(cause) => Status.Failure(cause)
        case _ => effect
      }
      logger.debug(s"Send acknowledgement: initiator=$initiator msg=$msg")
      initiator ! msg
    }
  }

  /** Inform the [[InstanceTrackerActor]] of a task state change (after persistence). */
  private[impl] case class StateChanged(ack: Ack)

  private[tracker] class ActorMetrics {
    // We can't use Metrics as we need custom names for compatibility.
    val stagedCount: AtomicGauge = AtomicGauge("service.mesosphere.marathon.task.staged.count")
    val runningCount: AtomicGauge = AtomicGauge("service.mesosphere.marathon.task.running.count")

    def resetMetrics(): Unit = {
      stagedCount.setValue(0)
      runningCount.setValue(0)
    }
  }

  private case class RepositoryStateUpdated(ack: Ack)
}

/**
  * Holds the current in-memory version of all task state. It gets informed of task state changes
  * after they have been persisted.
  *
  * It also spawns the [[InstanceUpdateActor]] as a child and forwards update operations to it.
  */
private[impl] class InstanceTrackerActor(
    metrics: InstanceTrackerActor.ActorMetrics,
    instanceLoader: InstancesLoader,
    updateStepProcessor: InstanceTrackerUpdateStepProcessor,
    instanceUpdaterProps: ActorRef => Props,
    repository: InstanceRepository) extends Actor with Stash with StrictLogging {

  private[this] val updaterRef = context.actorOf(instanceUpdaterProps(self), "updater")

  // Internal state of the tracker. It is set after initialization.
  var instancesBySpec: InstanceTracker.InstancesBySpec = _
  var counts: TaskCounts = _

  override val supervisorStrategy = OneForOneStrategy() { case _: Exception => Escalate }

  override def preStart(): Unit = {
    super.preStart()

    logger.info(s"${getClass.getSimpleName} is starting. Task loading initiated.")
    metrics.resetMetrics()

    import akka.pattern.pipe
    import context.dispatcher
    instanceLoader.load().pipeTo(self)
  }

  override def postStop(): Unit = {
    metrics.resetMetrics()

    super.postStop()
  }

  override def receive: Receive = initializing

  private[this] def initializing: Receive = LoggingReceive.withLabel("initializing") {
    case initialInstances: InstanceTracker.InstancesBySpec =>
      logger.info("Instances loading complete.")

      instancesBySpec = initialInstances
      counts = TaskCounts(initialInstances.allInstances, healthStatuses = Map.empty)

      metrics.stagedCount.setValue(counts.tasksStaged.toLong)
      metrics.runningCount.setValue(counts.tasksRunning.toLong)

      context.become(initialized)

      unstashAll()

    case Status.Failure(cause) =>
      // escalate this failure
      throw new IllegalStateException("while loading tasks", cause)

    case stashMe: AnyRef =>
      stash()
  }

  @SuppressWarnings(Array("all"))
  private[this] def initialized: Receive = {

    LoggingReceive.withLabel("initialized") {
      case InstanceTrackerActor.List =>
        sender() ! instancesBySpec

      case InstanceTrackerActor.Get(instanceId) =>
        sender() ! instancesBySpec.instance(instanceId)

      case ForwardTaskOp(deadline, instanceId, instanceUpdateOp) =>
        val op = InstanceOpProcessor.Operation(deadline, sender(), instanceId, instanceUpdateOp)
        updaterRef.forward(InstanceUpdateActor.ProcessInstanceOp(op))

      case InstanceTrackerActor.StateChanged(ack) =>
        import context.dispatcher

        ack.effect match {
          case InstanceUpdateEffect.Update(instance, _, _) =>
            updateRepository(() => repository.store(instance), ack)
              .pipeTo(self)(sender())

          case InstanceUpdateEffect.Expunge(instance, _) =>
            logger.debug(s"Received expunge for ${instance.instanceId}")
            updateRepository(() => repository.delete(instance.instanceId), ack)
              .pipeTo(self)(sender())

          case _ =>
            Future.successful(RepositoryStateUpdated(ack)) // forward only
              .pipeTo(self)(sender())
        }

      case Status.Failure(e) =>
        // pipeTo self might cause failed future to be propagated as Status.Failure
        // this should happen only for fatal exceptions
        throw e

      case RepositoryStateUpdated(ack) =>
        val maybeChange: Option[InstanceChange] = ack.effect match {
          case InstanceUpdateEffect.Update(instance, oldInstance, events) =>
            updateApp(instance.runSpecId, instance.instanceId, newInstance = Some(instance))
            Some(InstanceUpdated(instance, lastState = oldInstance.map(_.state), events))

          case InstanceUpdateEffect.Expunge(instance, events) =>
            logger.debug(s"Received expunge for ${instance.instanceId}")
            updateApp(instance.runSpecId, instance.instanceId, newInstance = None)
            Some(InstanceDeleted(instance, lastState = None, events))

          case InstanceUpdateEffect.Noop(_) | InstanceUpdateEffect.Failure(_) =>
            None
        }

        val originalSender = sender()

        import context.dispatcher
        maybeChange.map { change =>
          updateStepProcessor.process(change).recover {
            case NonFatal(cause) =>
              // since we currently only use ContinueOnErrorSteps, we can simply ignore failures here
              logger.warn("updateStepProcessor.process failed", cause)
              Done
          }
        }.getOrElse(Future.successful(Done)).foreach { _ =>
          ack.sendAck()
          originalSender ! (())
        }
    }
  }

  private def updateRepository(repositoryFunc: () => Future[Done], ack: Ack)(implicit ec: ExecutionContext): Future[RepositoryStateUpdated] = {
    repositoryFunc()
      .recoverWith(tryToRecoverRepositoryFailure(ack.effect))
      .map(_ => RepositoryStateUpdated(ack))
      .recoverWith {
        // if we could not recover from repository failure, propagate the error
        case NonFatal(e) => Future.successful(RepositoryStateUpdated(Ack(ack.initiator, InstanceUpdateEffect.Failure(e))))
      }
  }

  /**
    * Update the state of an app or pod and its instances.
    *
    * @param appId Identifier of the app or pod to update.
    * @param instanceId The identifier of the instance that is removed, added or updated.
    * @param newInstance A new or updated instance, or none if it is expunged.
    */
  def updateApp(appId: PathId, instanceId: Instance.Id, newInstance: Option[Instance]): Unit = {
    val updatedAppInstances = newInstance match {
      case None => instancesBySpec.updateApp(appId)(_.withoutInstance(instanceId))
      case Some(instance) => instancesBySpec.updateApp(appId)(_.withInstance(instance))
    }

    val updatedCounts = {
      val oldInstance = instancesBySpec.instance(instanceId)
      // we do ignore health counts
      val oldTaskCount = TaskCounts(oldInstance.to[Seq], healthStatuses = Map.empty)
      val newTaskCount = TaskCounts(newInstance.to[Seq], healthStatuses = Map.empty)
      counts + newTaskCount - oldTaskCount
    }

    instancesBySpec = updatedAppInstances
    counts = updatedCounts

    // this is run on any state change
    metrics.stagedCount.setValue(counts.tasksStaged.toLong)
    metrics.runningCount.setValue(counts.tasksRunning.toLong)
  }

  /**
    * If we encounter failure, we try to reload the effected task to make sure that the taskTracker
    * is up-to-date. We signal failure to the sender if the state is not as expected.
    *
    * If reloading the tasks also fails, the operation does fail.
    *
    * This tries to isolate failures that only effect certain tasks, e.g. errors in the serialization logic
    * which are only triggered for a certain combination of fields.
    */
  private def tryToRecoverRepositoryFailure(effect: InstanceUpdateEffect)(
    implicit
    ec: ExecutionContext): PartialFunction[Throwable, Future[Done]] = {
    case NonFatal(e) =>
      effect match {
        case expunge: InstanceUpdateEffect.Expunge =>
          repository.get(expunge.instance.instanceId).map {
            case None => Done
            case _ => throw e
          }
        case update: InstanceUpdateEffect.Update =>
          repository.get(update.instance.instanceId).map {
            case Some(repositoryInstance) if repositoryInstance == update.instance => Done
            // update did not get through
            case _ => throw e
          }
        case _ => throw e // do not recover anything else
      }
  }
}
