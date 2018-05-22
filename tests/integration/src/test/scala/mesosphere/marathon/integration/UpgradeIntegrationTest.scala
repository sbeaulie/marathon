package mesosphere.marathon
package integration

import java.io.File
import java.net.URL
import java.nio.file.Files

import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.client.RequestBuilding.Get
import akka.stream.Materializer
import mesosphere.marathon.core.pod.{HostNetwork, MesosContainer, PodDefinition}
import mesosphere.marathon.integration.facades.ITEnrichedTask
import mesosphere.marathon.util.ZookeeperServerTest
import mesosphere.{AkkaIntegrationTest, WhenEnvSet}
import mesosphere.marathon.integration.setup._
import mesosphere.marathon.io.IO
import mesosphere.marathon.state.{PathId, PersistentVolume, PersistentVolumeInfo, VolumeMount}
import org.apache.commons.io.FileUtils
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.sys.process.Process

/**
  * This integration test starts older Marathon versions one after another and finishes this upgrade procedure with the
  * current build. In each step we verfiy that all apps are still up and running.
  */
class UpgradeIntegrationTest extends AkkaIntegrationTest with MesosClusterTest with ZookeeperServerTest with MarathonAppFixtures with Eventually {

  import PathId._

  val zkURL = s"zk://${zkServer.connectUri}/marathon-$suiteName"

  val marathon149 = Marathon149(suiteName = s"$suiteName-1-4-9", mesosMasterUrl, zkURL)
  val marathon156 = Marathon156(suiteName = s"$suiteName-1-5-6", mesosMasterUrl, zkURL)
  val marathon16322 = Marathon16322(suiteName = s"$suiteName-1-6-322", mesosMasterUrl, zkURL)
  val marathonCurrent = LocalMarathon(suiteName = s"$suiteName-current", masterUrl = mesosMasterUrl, zkUrl = zkURL)

  // Configure Mesos to provide the Mesos containerizer with Docker image support.
  override lazy val mesosConfig = MesosConfig(
    launcher = "linux",
    isolation = Some("filesystem/linux,docker/runtime"),
    imageProviders = Some("docker"))

  override def beforeAll(): Unit = {
    super.beforeAll()

    // Download Marathon releases
    marathon149.downloadAndExtract()
    marathon156.downloadAndExtract()
    marathon16322.downloadAndExtract()

    marathon149.marathonPackage.deleteOnExit()
    marathon156.marathonPackage.deleteOnExit()
    marathon16322.marathonPackage.deleteOnExit()
  }

  trait MarathonDownload {

    val marathonPackage: File
    val tarballName: String
    val downloadURL: URL

    def downloadAndExtract() = {
      val tarball = new File(marathonPackage, tarballName)
      logger.info(s"Downloading $tarballName to ${tarball.getCanonicalPath}")
      FileUtils.copyURLToFile(downloadURL, tarball)
      IO.extractTGZip(tarball, marathonPackage)
    }
  }

  case class Marathon149(suiteName: String, masterUrl: String, zkUrl: String)(
      implicit
      val system: ActorSystem, val mat: Materializer, val ctx: ExecutionContext, val scheduler: Scheduler) extends BaseMarathon with MarathonDownload {

    override val marathonPackage = Files.createTempDirectory("marathon-1.4.9").toFile
    override val tarballName = "marathon-1.4.9.tgz"
    override val downloadURL = new URL("https://downloads.mesosphere.com/marathon/releases/1.4.9/marathon-1.4.9.tgz")

    override val processBuilder = {
      val java = sys.props.get("java.home").fold("java")(_ + "/bin/java")
      val jar = new File(marathonPackage, "marathon-1.4.9/target/scala-2.11/marathon-assembly-1.4.9.jar").getCanonicalPath
      val cmd = Seq(java, "-Xmx1024m", "-Xms256m", "-XX:+UseConcMarkSweepGC", "-XX:ConcGCThreads=2") ++ akkaJvmArgs ++
        Seq(s"-DmarathonUUID=$uuid -DtestSuite=$suiteName", "-client", "-jar", jar) ++ args
      Process(cmd, workDir, sys.env.toSeq: _*)
    }
  }

  case class Marathon156(suiteName: String, masterUrl: String, zkUrl: String)(
      implicit
      val system: ActorSystem, val mat: Materializer, val ctx: ExecutionContext, val scheduler: Scheduler) extends BaseMarathon with MarathonDownload {

    override val marathonPackage = Files.createTempDirectory("marathon-1.5.6").toFile
    override val tarballName = "marathon-1.5.6.tgz"
    override val downloadURL = new URL("https://downloads.mesosphere.com/marathon/releases/1.5.6/marathon-1.5.6.tgz")

    override val processBuilder = {
      val bin = new File(marathonPackage, "marathon-1.5.6/bin/marathon").getCanonicalPath
      val cmd = Seq("bash", bin, "-J-Xmx1024m", "-J-Xms256m", "-J-XX:+UseConcMarkSweepGC", "-J-XX:ConcGCThreads=2") ++ akkaJvmArgs ++
        Seq(s"-DmarathonUUID=$uuid -DtestSuite=$suiteName") ++ args
      Process(cmd, workDir, sys.env.toSeq: _*)
    }
  }

  case class Marathon16322(suiteName: String, masterUrl: String, zkUrl: String)(
      implicit
      val system: ActorSystem, val mat: Materializer, val ctx: ExecutionContext, val scheduler: Scheduler) extends BaseMarathon with MarathonDownload {

    override val marathonPackage = Files.createTempDirectory("marathon-1.6.322").toFile
    override val tarballName = "marathon-1.6.322.tgz"
    override val downloadURL = new URL("https://downloads.mesosphere.com/marathon/releases/1.6.322/marathon-1.6.322-2bf46b341.tgz")

    override val processBuilder = {
      val bin = new File(marathonPackage, "marathon-1.6.322-2bf46b341/bin/marathon").getCanonicalPath
      val cmd = Seq("bash", bin, "-J-Xmx1024m", "-J-Xms256m", "-J-XX:+UseConcMarkSweepGC", "-J-XX:ConcGCThreads=2") ++ akkaJvmArgs ++
        Seq(s"-DmarathonUUID=$uuid -DtestSuite=$suiteName") ++ args
      Process(cmd, workDir, sys.env.toSeq: _*)
    }
  }

  "Ephemeral and persistent apps and pods" should {
    "survive an upgrade cycle" taggedAs WhenEnvSet(envVarRunMesosTests, default = "true") in {

      // Start apps in 1.4.9
      Given("A Marathon 1.4.9 is running")
      marathon149.start().futureValue
      (marathon149.client.info.entityJson \ "version").as[String] should be("1.4.9")

      And("new running apps in Marathon 1.4.9")
      val app_149_fail = appProxy(testBasePath / "app-149-fail", "v1", instances = 1, healthCheck = None)
      marathon149.client.createAppV2(app_149_fail) should be(Created)

      val app_149 = appProxy(testBasePath / "app-149", "v1", instances = 1, healthCheck = None)
      marathon149.client.createAppV2(app_149) should be(Created)

      patienceConfig
      eventually { marathon149 should have (runningTasksFor(app_149.id.toPath, 1)) }
      eventually { marathon149 should have (runningTasksFor(app_149_fail.id.toPath, 1)) }

      val originalApp149Tasks = marathon149.client.tasks(app_149.id.toPath).value
      val originalApp149FailedTasks = marathon149.client.tasks(app_149_fail.id.toPath).value

      When("Marathon 1.4.9 is shut down")
      marathon149.stop().futureValue

      And(s"App ${app_149_fail.id} fails")
      killTask("app-149-fail")

      // Pass upgrade to 1.5.6
      And("Marathon is upgraded to 1.5.6")
      marathon156.start().futureValue
      (marathon156.client.info.entityJson \ "version").as[String] should be("1.5.6")

      And("new apps in Marathon 1.5.6 are added")
      val app_156 = appProxy(testBasePath / "app-156", "v1", instances = 1, healthCheck = None)
      marathon156.client.createAppV2(app_156) should be(Created)

      val app_156_fail = appProxy(testBasePath / "app-156-fail", "v1", instances = 1, healthCheck = None)
      marathon156.client.createAppV2(app_156_fail) should be(Created)

      Then("All apps from 1.5.6 are running")
      eventually { marathon156 should have (runningTasksFor(app_156.id.toPath, 1)) }
      eventually { marathon156 should have (runningTasksFor(app_156_fail.id.toPath, 1)) }

      val originalApp156Tasks = marathon156.client.tasks(app_156.id.toPath).value
      val originalApp156FailedTasks = marathon156.client.tasks(app_156_fail.id.toPath).value

      And("All apps from 1.4.9 are still running")
      marathon156.client.tasks(app_149.id.toPath).value should contain theSameElementsAs (originalApp149Tasks)

      When("Marathon 1.5.6 is shut down")
      marathon156.stop().futureValue

      And(s"App ${app_156_fail.id} fails")
      killTask("app-156-fail")

      // Pass upgrade to 1.6.322
      And("Marathon is upgraded to 1.6.322")
      marathon16322.start().futureValue
      (marathon16322.client.info.entityJson \ "version").as[String] should be("1.6.322")

      And("new pods in Marathon 1.6.322 are added")
      val resident_pod_16322 = PodDefinition(
        id = testBasePath / "resident-pod-16322",
        containers = Seq(
          MesosContainer(
            name = "task1",
            exec = Some(raml.MesosExec(raml.ShellCommand("cd $MESOS_SANDBOX && echo 'start' >> pst1/foo && python -m SimpleHTTPServer $ENDPOINT_TASK1"))),
            resources = raml.Resources(cpus = 0.1, mem = 32.0),
            endpoints = Seq(raml.Endpoint(name = "task1", hostPort = Some(0))),
            volumeMounts = Seq(VolumeMount(Some("pst"), "pst1", true))
          )
        ),
        volumes = Seq(PersistentVolume(name = Some("pst"), persistent = PersistentVolumeInfo(size = 10L))),
        networks = Seq(HostNetwork),
        instances = 1,
        unreachableStrategy = state.UnreachableDisabled,
        upgradeStrategy = state.UpgradeStrategy(0.0, 0.0)
      )
      marathon16322.client.createPodV2(resident_pod_16322) should be(Created)
      val (resident_pod_16322_port, resident_pod_16322_address) = eventually {
        val status = marathon16322.client.status(resident_pod_16322.id)
        status should be(Stable)
        status.value.instances(0).containers(0).endpoints(0).allocatedHostPort should be('defined)
        val port = status.value.instances(0).containers(0).endpoints(0).allocatedHostPort.get
        (port, status.value.instances(0).networks(0).addresses(0))
      }

      Then(s"pod ${resident_pod_16322.id} can be queried on http://$resident_pod_16322_address:$resident_pod_16322_port")
      implicit val requestTimeout = 30.seconds
      eventually { AkkaHttpResponse.request(Get(s"http://$resident_pod_16322_address:$resident_pod_16322_port/pst1/foo")).futureValue.entityString should be("start\n") }

      Then("All apps from 1.4.9 and 1.5.6 are still running")
      marathon16322.client.tasks(app_149.id.toPath).value should contain theSameElementsAs (originalApp149Tasks)
      marathon16322.client.tasks(app_156.id.toPath).value should contain theSameElementsAs (originalApp156Tasks)

      // Pass upgrade to current
      When("Marathon is upgraded to the current version")
      marathon16322.stop().futureValue
      marathonCurrent.start().futureValue
      (marathonCurrent.client.info.entityJson \ "version").as[String] should be("1.6.0-SNAPSHOT")

      Then("All apps from 1.4.9 and 1.5.6 are still running")
      marathonCurrent.client.tasks(app_149.id.toPath).value should contain theSameElementsAs (originalApp149Tasks)
      marathonCurrent.client.tasks(app_156.id.toPath).value should contain theSameElementsAs (originalApp156Tasks)

      And("All apps from 1.4.9 and 1.5.6 are recovered and running again")
      eventually { marathonCurrent should have(runningTasksFor(app_149_fail.id.toPath, 1)) }
      marathonCurrent.client.tasks(app_149_fail.id.toPath).value should not contain theSameElementsAs(originalApp149FailedTasks)

      eventually { marathonCurrent should have(runningTasksFor(app_156_fail.id.toPath, 1)) }
      marathonCurrent.client.tasks(app_156_fail.id.toPath).value should not contain theSameElementsAs(originalApp156FailedTasks)

      And("All pods from 1.6.322 are still running")
      eventually { marathonCurrent.client.status(resident_pod_16322.id) should be(Stable) }
      eventually { AkkaHttpResponse.request(Get(s"http://$resident_pod_16322_address:$resident_pod_16322_port/pst1/foo")).futureValue.entityString should be("start\n") }

      marathonCurrent.close()
    }
  }

  def killTask(appName: String): Unit = {
    val pidPattern = """([^\s]+)\s+([^\s]+)\s+.*""".r
    val pids = Process("ps aux").!!.split("\n").filter { process =>
      process.contains("src/test/python/app_mock.py") && process.contains(appName)
    }.collect {
      case pidPattern(_, pid) => pid
    }

    Process(s"kill -9 ${pids.mkString(" ")}").!
    logger.info(s"Killed tasks of app $appName with PIDs ${pids.mkString(" ")}")
  }

  /**
    * Scala [[HavePropertyMatcher]] that checks that numberOfTasks are in running state for app appId on given Marathon.
    *
    * Do not use the class directly but [[UpgradeIntegrationTest.runningTasksFor]]:
    *
    * {{{
    *   marathon149 should have(runningTasksFor(app_149.id.toPath, 2))
    * }}}
    *
    * @param appId The app the is checked for running tasks.
    * @param numberOfTasks The number of tasks that should be running.
    */
  class RunningTasksMatcher(appId: PathId, numberOfTasks: Int) extends HavePropertyMatcher[BaseMarathon, List[ITEnrichedTask]] {
    def apply(marathon: BaseMarathon): HavePropertyMatchResult[List[ITEnrichedTask]] = {
      val tasks = marathon.client.tasks(appId).value
      val notRunningTasks = tasks.filter(_.state != "TASK_RUNNING")
      val matches = tasks.size == numberOfTasks && notRunningTasks.size == 0
      HavePropertyMatchResult(matches, "runningTasks", List.empty, notRunningTasks)
    }
  }

  def runningTasksFor(appId: PathId, numberOfTasks: Int) = new RunningTasksMatcher(appId, numberOfTasks)

  override val testBasePath = PathId("/")
  override val healthCheckPort: Int = 0
}
