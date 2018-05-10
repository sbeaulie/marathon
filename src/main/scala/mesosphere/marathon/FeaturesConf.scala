package mesosphere.marathon

import org.rogach.scallop.ScallopConf

trait FeaturesConf extends ScallopConf {
  lazy val features = opt[String](
    "enable_features",
    descr = s"A comma-separated list of features. Available features are: ${Features.description}",
    required = false,
    default = None,
    noshort = true,
    validate = validateFeatures
  )

  lazy val availableFeatures: Set[String] = features.get.map(parseFeatures).getOrElse(Set.empty)

  private[this] def validateFeatures(str: String): Boolean = {
    val parsed = parseFeatures(str)
    // throw exceptions for better error messages
    val unknownFeatures = parsed.filter(!Features.availableFeatures.contains(_))
    lazy val unknownFeaturesString = unknownFeatures.mkString(", ")
    require(
      unknownFeatures.isEmpty,
      s"Unknown features specified: $unknownFeaturesString. Available features are: ${Features.description}"
    )
    true
  }

  def isFeatureSet(name: String): Boolean = availableFeatures.contains(name)

  private[this] def parseFeatures(str: String): Set[String] =
    str.split(',').map(_.trim).filter(_.nonEmpty).toSet
}
