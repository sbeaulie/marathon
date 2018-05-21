package mesosphere.marathon
package api

import akka.actor.ActorSystem
import com.google.inject.AbstractModule
import javax.inject.Named

import com.google.inject.{Provides, Scopes, Singleton}
import mesosphere.marathon.io.SSLContextUtil
import mesosphere.marathon.MarathonConf
import mesosphere.marathon.api.forwarder.{AsyncUrlConnectionRequestForwarder, RequestForwarder, JavaUrlConnectionRequestForwarder}
import scala.concurrent.ExecutionContext

/**
  * Setup the dependencies for the LeaderProxyFilter.
  * This filter will redirect to the master if running in HA mode.
  */
class LeaderProxyFilterModule extends AbstractModule {
  override def configure(): Unit = {
  }

  @Provides
  @Singleton
  def provideRequestForwarder(
    httpConf: HttpConf,
    featuresConf: FeaturesConf,
    leaderProxyConf: LeaderProxyConf,
    @Named(ModuleNames.HOST_PORT) myHostPort: String)(implicit executionContext: ExecutionContext, actorSystem: ActorSystem): RequestForwarder = {
    val sslContext = SSLContextUtil.createSSLContext(httpConf.sslKeystorePath.get, httpConf.sslKeystorePassword.get)
    if (featuresConf.isDeprecatedFeatureSet(DeprecatedFeatures.syncProxy))
      new JavaUrlConnectionRequestForwarder(sslContext, leaderProxyConf, myHostPort)
    else
      new AsyncUrlConnectionRequestForwarder(sslContext, leaderProxyConf, myHostPort)
  }
}

class MarathonRestModule() extends AbstractModule {

  override def configure(): Unit = {
    // Map some exceptions to HTTP responses
    bind(classOf[MarathonExceptionMapper]).asEagerSingleton()

    // Service API
    bind(classOf[SystemResource]).in(Scopes.SINGLETON)

    // V2 API
    bind(classOf[v2.AppsResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.PodsResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.TasksResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.QueueResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.GroupsResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.InfoResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.LeaderResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.DeploymentsResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.SchemaResource]).in(Scopes.SINGLETON)
    bind(classOf[v2.PluginsResource]).in(Scopes.SINGLETON)

    bind(classOf[CORSFilter]).asEagerSingleton()
    bind(classOf[CacheDisablingFilter]).asEagerSingleton()
    bind(classOf[WebJarServlet]).in(Scopes.SINGLETON)
    bind(classOf[PublicServlet]).in(Scopes.SINGLETON)
  }

  @Provides
  @Singleton
  def provideRequestsLimiter(conf: MarathonConf): LimitConcurrentRequestsFilter = {
    new LimitConcurrentRequestsFilter(conf.maxConcurrentHttpConnections.get)
  }

  @Provides
  @Singleton
  def rootApplication(
    marathonExceptionMapper: MarathonExceptionMapper,
    systemResource: SystemResource,
    appsResource: v2.AppsResource,
    podsResource: v2.PodsResource,
    tasksResource: v2.TasksResource,
    queueResource: v2.QueueResource,
    groupsResource: v2.GroupsResource,
    infoResource: v2.InfoResource,
    leaderResource: v2.LeaderResource,
    deploymentsResource: v2.DeploymentsResource,
    schemaResource: v2.SchemaResource,
    pluginsResource: v2.PluginsResource): RootApplication = {
    new RootApplication(
      Seq(marathonExceptionMapper),
      Seq(systemResource, appsResource, podsResource, tasksResource, queueResource,
        groupsResource, infoResource, leaderResource, deploymentsResource, schemaResource, pluginsResource))
  }
}
