import sbt.Keys._
import sbt._
import com.typesafe.sbt.packager.SettingsHelper._
import java.lang.System

import com.typesafe.sbt.packager.SettingsHelper
import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

updateOptions := updateOptions.value.withCachedResolution(true)

import scala.io.Source
import scala.util.control.Exception._

lazy val omlProductDir = settingKey[File](
  "Location of the gov.nasa.jpl.imce.oml.runtime.platform.updatesite's plugin folder")

lazy val extractOMLProduct =
  taskKey[PathFinder]("Extract the OML platform update site to a folder")

lazy val core = Project("omlConverters", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(UniversalPlugin)
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2017",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,
    // 'omlConverter' will be a command-line script to run
    // the single application, gov.nasa.jpl.imce.oml.converters.OMLConverter
    executableScriptName := "omlConverter",

    // Needed to transitively get dependencies from the gov.nasa.jpl.imce:imce.third_party.* zip aggregates
    classpathTypes += "zip",

    SettingsHelper.makeDeploymentSettings(Universal, packageZipTarball in Universal, "tgz"),

    SettingsHelper.makeDeploymentSettings(UniversalDocs, packageXzTarball in UniversalDocs, "tgz"),

    packagedArtifacts in publish += {
      val p = (packageZipTarball in Universal).value
      val n = (name in Universal).value
      Artifact(n, "tgz", "tgz", Some("resource"), Seq(), None, Map()) -> p
    },
    packagedArtifacts in publishLocal += {
      val p = (packageZipTarball in Universal).value
      val n = (name in Universal).value
      Artifact(n, "tgz", "tgz", Some("resource"), Seq(), None, Map()) -> p
    },
    packagedArtifacts in publishM2 += {
      val p = (packageZipTarball in Universal).value
      val n = (name in Universal).value
      Artifact(n, "tgz", "tgz", Some("resource"), Seq(), None, Map()) -> p
    },
    buildInfoPackage := "gov.nasa.jpl.imce.oml.converters",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") {
      buildUTCDate.value
    }),
    projectID := {
      val previous = projectID.value
      previous.extra("build.date.utc" -> buildUTCDate.value,
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
        Artifact("gov.nasa.jpl.imce.oml.runtime.platform.updatesite",
                 "zip",
                 "zip"),
      "org.eclipse.platform" % "org.eclipse.core.runtime" % "3.12.0",
      "org.eclipse.platform" % "org.eclipse.core.resources" % "3.11.1",
      "org.eclipse.emf" % "org.eclipse.emf.codegen.ecore" % "2.12.0",
      "org.eclipse.emf" % "org.eclipse.emf.codegen.ecore.xtext" % "1.2.0",
      "org.eclipse.emf" % "org.eclipse.emf.mwe2.runtime" % "2.9.+",
      "org.eclipse.xtext" % "org.eclipse.xtext.ecore" % "2.11.0",
      "org.eclipse.xtext" % "org.eclipse.xtext.xbase" % "2.11.0",
      "org.eclipse.xtext" % "org.eclipse.xtext.xbase.lib" % "2.11.0"
    ),
    // Avoid unresolvable dependencies from old versions of log4j
    libraryDependencies ~= {
      _ map {
        case m if m.organization == "log4j" =>
          m.exclude("javax.jms", "jms")
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
          if artifact.name.startsWith(
            "gov.nasa.jpl.imce.oml.runtime.platform.updatesite")
          if artifact.extension == "zip"
        } yield {
          s.log.warn("... found! Extracting.")
          val files = IO.unzip(archive, upd)
          require(files.nonEmpty)
          s.log.warn(s"Extracted ${files.size} files from $archive")
          ()
        }
      }

      val plugins = upd / "plugins"

      // @see https://github.com/JPL-IMCE/gov.nasa.jpl.imce.oml.tycho/issues/15
      IO.delete((plugins / "org.eclipse.emf.cdo.ecore.retrofit*").get)

      val jars =
          plugins ** "org.eclipse.emf.cdo_4.5.0.*.jar" +++
          plugins ** "org.eclipse.emf.cdo.common_4.5.0.*.jar" +++
          plugins ** "org.eclipse.net4j.util_3.6.0.*.jar" +++
          plugins ** "org.eclipse.emf.ecore.xcore_1.4.0.*.jar" +++
          plugins ** "org.eclipse.emf.ecore.xcore.lib_1.1.0.*.jar" +++
          plugins ** "org.eclipse.emf.codegen_2.11.0.*.jar"

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
        % "compile" withSources)
  )
