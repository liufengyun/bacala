package bacala.ivy

import java.io.{InputStream, ByteArrayInputStream}
import java.net.URL
import org.apache.ivy.plugins.parser.xml.XmlModuleDescriptorParser
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.repository.BasicResource
import org.apache.ivy.plugins.repository.url.URLResource
import org.apache.ivy.core.module.descriptor.{ExcludeRule, DependencyDescriptor, Configuration, Artifact}

/** Parses Ivy XML file using the Ivy library
  */
object IvyParser extends (String => IDescriptor) {
  val parser = XmlModuleDescriptorParser.getInstance()
  val setting = new IvySettings()

  override def apply(uri: String) = {
    val inputLoc = new URL(uri)
    val resource = new org.apache.ivy.plugins.repository.url.URLResource(inputLoc)
    val md = parser.parseDescriptor(setting, inputLoc, resource, false)

    val moduleRevisionId = md.getModuleRevisionId

    val lib = ILib(moduleRevisionId.getOrganisation, moduleRevisionId.getName)
    val pkg = IPackage(lib, moduleRevisionId.getRevision)

    val confs = md.getConfigurations.toSet map { (conf: Configuration) =>
      IConf(conf.getName, conf.getDescription, conf.getVisibility.toString,
        conf.getExtends.toSeq, conf.isTransitive, conf.getDeprecated)
    }

    val artfs = md.getAllArtifacts.toSet map { (artf: Artifact) =>
      val url = artf.getUrl
      IArtifact(artf.getName, artf.getType, artf.getExt, artf.getConfigurations.toSeq,
        if (url == null) "" else url.toString)
    }

    val excludes = md.getAllExcludeRules.toSet.map(toExclude)

    val deps = md.getDependencies.toSeq.map(toDep)

    IDescriptor(pkg, confs, deps, artfs, excludes)
  }

  def toExclude(rule: ExcludeRule): IExclude = {
      val artifactId = rule.getId
      val moduleId = artifactId.getModuleId
      val lib = ILib(moduleId.getOrganisation, moduleId.getName)
      IExclude(lib, artifactId.getName, artifactId.getType, artifactId.getExt,
        rule.getMatcher.getName, rule.getConfigurations.toSeq)
  }

  def toDep(dep: DependencyDescriptor): IDependency = {
    val moduleId = dep.getDependencyId
    val lib = ILib(moduleId.getOrganisation, moduleId.getName)
    val version = dep.getDependencyRevisionId.getRevision
    val versionConstraint = dep.getDynamicConstraintDependencyRevisionId.getRevision
    val transitive = dep.isTransitive
    val force = dep.isForce
    val changing = dep.isChanging

    val depArtfs = dep.getAllDependencyArtifacts.toSeq map { artf =>
      val url = artf.getUrl
      IDepArtifact(artf.getName, artf.getType, artf.getExt,
        artf.getConfigurations.toSeq, if (url == null) "" else url.toString)
    }

    val excludes = dep.getAllExcludeRules.toSeq.map(toExclude)

    val includes = dep.getAllIncludeRules.toSeq map { rule =>
      val artifactId = rule.getId
      val moduleId = artifactId.getModuleId
      IInclude(artifactId.getName, artifactId.getType, artifactId.getExt,
        rule.getMatcher.getName, rule.getConfigurations.toSeq)
    }

    val map = (dep.getModuleConfigurations.toSeq map { conf =>
      conf -> dep.getDependencyConfigurations(conf).toSeq
    }).toMap

    IDependency(lib, version, versionConstraint, transitive, force,
      changing, depArtfs, excludes, includes, map)
  }
}
