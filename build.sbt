import sbt.Keys._
import sbt._
import com.typesafe.sbt.packager.SettingsHelper._
import java.lang.System

import com.typesafe.sbt.packager.SettingsHelper
import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

import scala.languageFeature.postfixOps

updateOptions := updateOptions.value.withCachedResolution(true)

import scala.io.Source
import scala.util.control.Exception._

lazy val omlProductDir = settingKey[File](
  "Location of the gov.nasa.jpl.imce.oml.runtime.platform.updatesite's plugin folder")

lazy val extractOMLProduct =
  taskKey[PathFinder]("Extract the OML platform update site to a folder")

lazy val omlConverters = Project("omlConverters", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(UniversalPlugin)
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2017",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,
    // 'omlConverter' will be a command-line script to run
    // the single application, gov.nasa.jpl.imce.oml.converters.OMLConverter
    mainClass in Compile := Some("gov.nasa.jpl.imce.oml.converters.OMLConverter"),

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
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.platform.updatesite" % Versions_oml_core.version
        % "provided"
        artifacts Artifact("gov.nasa.jpl.imce.oml.platform.updatesite", "zip", "zip"),
      "org.eclipse.platform" % "org.eclipse.core.runtime" % "3.12.0",
      "org.eclipse.platform" % "org.eclipse.core.resources" % "3.11.1",
      "org.eclipse.emf" % "org.eclipse.emf.codegen.ecore" % "2.12.0",
      "org.eclipse.emf" % "org.eclipse.emf.codegen.ecore.xtext" % "1.2.0",
      "org.eclipse.emf" % "org.eclipse.emf.mwe2.runtime" % "2.9.+",
      "org.eclipse.xtext" % "org.eclipse.xtext.ecore" % "2.11.0",
      "org.eclipse.xtext" % "org.eclipse.xtext.xbase" % "2.11.0",
      "org.eclipse.xtext" % "org.eclipse.xtext.xbase.lib" % "2.11.0"
    ),

    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch),

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % Versions_circe.version),

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
    mappings in Universal := {
      val prev = (mappings in Universal).value
      prev.filterNot { case (f, n) =>
        n.endsWith("-resource.jar") ||
        (f.name.endsWith("log4j-1.2.17.zip") && n == "lib/log4j.log4j-1.2.17.jar") ||
          n == "lib/ch.qos.logback.logback-classic-1.0.7.jar" ||
          n == "lib/ch.qos.logback.logback-core-1.0.7.jar"
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
          if c.configuration == "provided"
          m <- c.modules
          (artifact, archive) <- m.artifacts
          if artifact.name.startsWith(
            "gov.nasa.jpl.imce.oml.platform.updatesite")
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

      val jars =
          plugins ** "org.eclipse.emf.cdo_4.5.0.*.jar" +++
          plugins ** "org.eclipse.emf.cdo.common_4.5.0.*.jar" +++
          plugins ** "org.eclipse.net4j.util_3.6.0.*.jar" +++
          plugins ** "org.eclipse.emf.ecore.xcore_1.4.0.*.jar" +++
          plugins ** "org.eclipse.emf.ecore.xcore.lib_1.1.0.*.jar" +++
          plugins ** "org.eclipse.emf.codegen_2.11.0.*.jar"

      // Delete unused files.
      val other = ((upd ** "*") --- upd --- plugins --- jars).get
      s.log.info(s"other: ${other.size}")
      other.foreach { f =>
        if (f.isFile)
          IO.delete(f)
        s.log.info(s"=> $f")
      }

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
        % "compile" withSources())
  )
  .dependsOn(graphMisc % "compile")

// temporary, see: https://github.com/scala-graph/scala-graph/issues/74

val graph_isSnapshot = false
val graph_snapshot = if (graph_isSnapshot) "-SNAPSHOT" else ""
val graph_major = "1.11"
val graph_all         = s"$graph_major.0$graph_snapshot"
val graph_core        = s"$graph_major.5$graph_snapshot"
val graph_constrained = s"$graph_major.0$graph_snapshot"
val graph_dot         = s"$graph_major.5$graph_snapshot"
val graph_json        = s"$graph_major.0$graph_snapshot"
val graph_misc        = s"$graph_major.0$graph_snapshot"

lazy val graph_defaultSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "org.scala-graph",
  parallelExecution in Test := false,
  scalaVersion := "2.11.8",
  scalacOptions in(Compile, doc) ++=
    Opts.doc.title(name.value) ++
      Opts.doc.version(version.value),
  // prevents sbteclipse from including java source directories
  unmanagedSourceDirectories in Compile := (scalaSource in Compile) (Seq(_)).value,
  unmanagedSourceDirectories in Test := (scalaSource in Test) (Seq(_)).value,
  scalacOptions in(Compile, doc) ++= List("-diagrams", "-implicits"),
  scalacOptions in(Compile, doc) ++= (baseDirectory map { d =>
    Seq("-doc-root-content", d / "rootdoc.txt" getPath)
  }).value,
  autoAPIMappings := true,

  resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
  scalacOptions += "-Xplugin-disable:artima-supersafe",

  testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.12" % "test",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.5" % "test")
)

lazy val graphCore = Project(
  id = "Graph-core",
  base = file("scala-graph/core"),
  settings = graph_defaultSettings ++ Seq(
    name := "Graph Core",
    version := graph_core,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "optional;provided"
  )
)

lazy val graphMisc = Project(
  id = "Graph-misc",
  base = file("scala-graph/misc"),
  settings = graph_defaultSettings ++ Seq(
    name := "Graph Miscellaneous",
    version := graph_misc,
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.7"
  )
) dependsOn graphCore
