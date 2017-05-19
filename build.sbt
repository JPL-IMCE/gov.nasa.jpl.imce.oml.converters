
import sbt.Keys._
import sbt._
import com.typesafe.sbt.packager.SettingsHelper._

import java.lang.System
import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

updateOptions := updateOptions.value.withCachedResolution(true)

import scala.io.Source
import scala.util.control.Exception._

lazy val omlProductDir = settingKey[File]("Location of the gov.nasa.jpl.imce.oml.runtime.platform.updatesite's plugin folder")

lazy val extractOMLProduct = taskKey[PathFinder]("Extract the OML platform update site to a folder")

lazy val core = Project("omlConverters", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(UniversalDeployPlugin)
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2017",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    // 'omlConverter' will be a command-line script to run
    // the single application, gov.nasa.jpl.imce.oml.converters.OMLConverter
    executableScriptName := "omlConverter",

    packagedArtifacts in publish += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    },

    packagedArtifacts in publishLocal += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    },

    packagedArtifacts in publishM2 += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    },

    buildInfoPackage := "gov.nasa.jpl.imce.oml.converters",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),

    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    scalacOptions in (Compile, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Test, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Compile, doc) += "-Xplugin-disable:artima-supersafe",
    scalacOptions in (Test, doc) += "-Xplugin-disable:artima-supersafe",

    scalacOptions += "-g:vars",

    libraryDependencies ++= Seq(
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.dsl" % Versions_oml_core.version,
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.model" % Versions_oml_core.version,
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.runtime.platform.updatesite"
        % "0.7.0.1" artifacts
        Artifact("gov.nasa.jpl.imce.oml.runtime.platform.updatesite", "zip", "zip")
    ),

    // Avoid unresolvable dependencies from old versions of log4j
    libraryDependencies ~= {
      _ map {
        case m if m.organization == "log4j" =>
          m
            .exclude("javax.jms", "jms")
            .exclude("com.sun.jmx", "jmxri")
            .exclude("com.sun.jdmk", "jmxtools")
        case m => m
      }
    },

    omlProductDir := baseDirectory.value / "target" / "omlProduct",

    extractOMLProduct := {
      val upd: File = omlProductDir.value
      val s = streams.value

      if (!upd.exists) {
        s.log.warn("Looking for OML product...")
        for {
          c <- update.value.configurations
          if c.configuration == "compile"
          m <- c.modules
          (artifact, archive) <- m.artifacts
          if artifact.name.startsWith("gov.nasa.jpl.imce.oml.runtime.platform.updatesite")
          if artifact.extension == "zip"
        } yield {
          s.log.warn("... found! Extracting.")
          val files = IO.unzip(archive, upd)
          require(files.nonEmpty)
          s.log.warn(s"Extracted ${files.size} files from $archive")
          ()
        }
      }

      // @see https://github.com/JPL-IMCE/gov.nasa.jpl.imce.oml.tycho/issues/15
      IO.delete((upd / "plugins" / "org.eclipse.emf.cdo.ecore.retrofit*").get)

      val jars = ( upd / "plugins" ) ** "*.jar"
      jars
    },

    unmanagedJars in Compile := extractOMLProduct.value.classpath,

    unmanagedJars in (Compile, doc) := extractOMLProduct.value.classpath
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omf-scala-binding-owlapi",
    "gov.nasa.jpl.omf.scala.binding.owlapi",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.binding.owlapi"
        % Versions_omf_owlapi.version
        % "compile" withSources() artifacts(
        Artifact("gov.nasa.jpl.omf.scala.binding.owlapi"),
        Artifact("gov.nasa.jpl.omf.scala.binding.owlapi", "zip", "zip", "resource"))
    )
  )