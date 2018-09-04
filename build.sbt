import sbt.Keys.{artifacts, _}
import sbt._
import java.lang.System

import com.typesafe.sbt.packager.SettingsHelper
import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

import scala.languageFeature.postfixOps

updateOptions := updateOptions.value.withCachedResolution(true)

import scala.io.Source
import scala.util.control.Exception._

lazy val omlProductRepo = settingKey[String](
  "Location of the repository where to resolve the OML Product linux.gtk.x86_64.tar.gz"
)

lazy val omlProductDir = settingKey[File](
  "Location of the gov.nasa.jpl.imce.oml.runtime.platform.updatesite's plugin folder")

lazy val extractOMLProduct =
  taskKey[PathFinder]("Extract the OML platform update site to a folder")

val owlapiLibs = taskKey[Seq[Attributed[File]]]("OWLAPI libraries")

val resources: Configuration = Configurations.config("resources")

// For building using a local snapshot build of OML:
//
// 1) in project/Versions_oml_core.scala, set version accordingly, e.g., "0.9.0-SNAPSHOT"
//
// 2) sbt -DOML_PRODUCT_REPO=file://$HOME/.m2/repository
//
// 3) extractOMLProduct
//

val showMappings = taskKey[Unit]("showMappings")

showMappings := {
  val pairs = (mappings in Universal).value.sortBy(_._2)
  pairs.foreach {
    case (file, path) => println(file + " -> " + path)
  }
}

lazy val omlConverters = Project("omlConverters", file("."))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(UniversalDeployPlugin)
  .enablePlugins(LauncherJarPlugin)
  .enablePlugins(IMCEGitPlugin)
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2017",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    omlProductRepo := Option.apply(System.getProperty("OML_PRODUCT_REPO")).getOrElse("https://dl.bintray.com/jpl-imce/gov.nasa.jpl.imce.oml"),

    buildInfoPackage := "gov.nasa.jpl.imce.oml.converters",

    compile.in(Compile) := (compile.in(Compile) dependsOn buildInfo.in(Compile)).value,

    scalaVersion := Versions.scala,

    topLevelDirectory := Some("OMLConverters"),

    // 'omlDirectoryConverter' will be a command-line script to run
    // the single application, gov.nasa.jpl.imce.oml.processor.OMLConverter
    mainClass in Compile := Some("gov.nasa.jpl.imce.oml.converters.OMLConverter"),

    executableScriptName := "omlConverter",

    // skip doc on stage
    mappings in (Compile, packageDoc) := Seq(),

    // Needed to transitively get dependencies from the gov.nasa.jpl.imce:imce.third_party.* zip aggregates
    classpathTypes += "zip",
    classpathTypes += "linux.gtk.x86_64.tar.gz",

    SettingsHelper.makeDeploymentSettings(Universal, packageZipTarball in Universal, "tgz"),

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

    resolvers += Resolver.mavenLocal,
    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce.oml"),

    resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases",
    scalacOptions in (Compile, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Test, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits", "-Xplugin-disable:artima-supersafe"),
    scalacOptions in (Test, doc) ++= Seq("-groups", "-implicits", "-Xplugin-disable:artima-supersafe"),
    scalacOptions += "-g:vars",

    scalacOptions in (Compile,doc) ++= Seq(
      "-diagrams",
      "-doc-title", name.value,
      "-doc-root-content", baseDirectory.value + "/rootdoc.txt"),

    libraryDependencies += "gov.nasa.jpl.imce" %% "gov.nasa.jpl.imce.xml.catalog.scope" % Versions_xml_catalog_scope.version,

    libraryDependencies ++= Seq(
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.dsl" % Versions_oml_core.version,
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.model" % Versions_oml_core.version,
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.zip" % Versions_oml_core.version,
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.product" % Versions_oml_core.version
        artifacts
        Artifact("gov.nasa.jpl.imce.oml.product", "linux.gtk.x86_64.tar.gz", "linux.gtk.x86_64.tar.gz")
          .withUrl(Some(url(
            s"${omlProductRepo.value}/gov/nasa/jpl/imce/oml/gov.nasa.jpl.imce.oml.product/${Versions_oml_core.version}/gov.nasa.jpl.imce.oml.product-${Versions_oml_core.version}-linux.gtk.x86_64.tar.gz")))
    ),
    dependencyOverrides += "com.fasterxml.jackson.module" % "jackson-module-paranamer" % Versions.spark_jackson % "compile",
    dependencyOverrides += "com.fasterxml.jackson.module" %% "jackson-module-scala" % Versions.spark_jackson % "compile",

    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch),

    libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.+",

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

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",

    libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.11.0",
    libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.11.0",
    libraryDependencies += "org.apache.logging.log4j" % "log4j-iostreams" % "2.11.0",
    libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.11.0",

    mappings in Universal := {
      val s = streams.value
      val prev = (mappings in Universal).value
      s.log.warn(s"universal:mappings => ${prev.size} entries...")
      var oks: Int = 0
      var exs: Int = 0
      val result = prev.filterNot { case (f, n) =>
        val ok = f.name.endsWith(".tar.gz") ||
          f.name.endsWith("-resource.zip") ||
          n.contains("test") ||
          (f.name.endsWith("log4j-1.2.17.zip") && n == "lib/log4j.log4j-1.2.17.jar") ||
          n == "lib/ch.qos.logback.logback-classic-1.0.7.jar" ||
          n == "lib/ch.qos.logback.logback-core-1.0.7.jar" ||
          n.startsWith("lib/owlapi-distribution-")
        if (ok) exs += 1 else oks += 1
        ok
      }
      s.log.warn(s"universal:mappings => ${prev.size} entries (kept $oks; removed $exs) => ${result.size} filtered")
      result
    },
    omlProductDir := baseDirectory.value / "target" / "omlProduct",
    extractOMLProduct := {
      val upd: File = omlProductDir.value
      val s = streams.value

      // This logic works in the case where there are no SBT sub-projects
      // (i.e., there is no 'links.sbt' file  ).
      // In this case, there is only 1 SBT project, omlConverters.
      // Finding the 'gov.nasa.jpl.imce.oml.product' dependency artifact works.

      if (!upd.exists) {
        s.log.warn("Looking for OML product...")
        for {
          c <- update.value.configurations
          if c.configuration == "compile"
          m <- c.modules
          if m.module.organization == "gov.nasa.jpl.imce.oml"
          _ = s.log.warn(s"${c.configuration} ${m.module.organization} % ${m.module.name} => ${m.artifacts.size} artifacts")
          (artifact, archive) <- m.artifacts
          _ = s.log.warn(s"${c.configuration}: artifact: name=${artifact.name} cls=${artifact.classifier} ext=${artifact.extension}")
          if artifact.name.startsWith("gov.nasa.jpl.imce.oml.product")
          if artifact.extension.contains("gz")
          url <- artifact.url
        } yield {
          s.log.warn(s"url protocol:${url.getProtocol} path:${url.getPath}")
          s.log.warn(s"artifact: ${archive.getAbsolutePath}")
          val artifactPath = if ("file" == url.getProtocol)
            url.getPath
          else
            archive.getAbsolutePath

          s.log.warn(s"... found! Extracting from $artifactPath")
          upd.mkdirs()
          Process(Seq("tar", "-zxf", artifactPath), Some(upd)).! match {
            case 0 =>
              s.log.warn(s"=> Extracted ${artifact.name} from $artifactPath")
            case n =>
              sys.error(s"Error extracting $artifactPath; exit code: $n")
          }
          ()
        }
      }

      // Finding the 'gov.nasa.jpl.imce.oml.product' dependency artifact fails
      // precisely when there are SBT sub-projects via the 'links.sbt' trick.
      // In that case, we copy the dependency artifact directly from its'
      // repository URL location since we can't get this information from SBT's dependencies.

      // This is an ugly hack but it workss.

      if (!upd.exists) {
        upd.mkdirs()

        val omlProductURL = url(
          s"${omlProductRepo.value}/gov/nasa/jpl/imce/oml/gov.nasa.jpl.imce.oml.product/${Versions_oml_core.version}/gov.nasa.jpl.imce.oml.product-${Versions_oml_core.version}-linux.gtk.x86_64.tar.gz")
        s.log.warn(s"Directly copying artifact from omlProductURL=$omlProductURL")

        val omlArchive = upd / "oml.tar.gz"
        (omlProductURL #> omlArchive).! match {
          case 0 =>
            Process(Seq("tar", "-zxf", omlArchive.getAbsolutePath), Some(upd)).! match {
              case 0 =>
                s.log.warn(s"=> Extracted ${omlArchive.getName} from $omlArchive")
              case n =>
                sys.error(s"Error extracting $omlArchive; exit code: $n")
            }

          case n =>
            sys.error(s"Error copying $omlProductURL; exit code: $n")
        }


        ()
      }

      // Eclipse is astonishingly painful for Maven-centric development with for several reasons:
      // 1) Maven/Tycho in the so-called "manifest-first" paradigm produces POM files that
      //    lack dependency information. It is unclear how to switch to the "pom-first" alternative
      //    paradigm since it requires generating the metadata files that Eclipse needs.
      // 2) It is unclear whether Eclipse.org has any official policy for publishing
      //    Maven artifacts about Eclipse plugins/features. Some of the Eclipse.org artifacts
      //    on Maven central are stale.
      // 3) Eclipse.org provides several P2 repositories with Eclipse plugins/features;
      //    however, finding what's in these P2 repositories is anything but developer-friendly.
      //    Some P2 repos do not provide a web UI view for browsing their contents, others do.
      //    This variation means that one cannot rely on having a web UI for browsing arbitrary Eclipse.org repos.
      // 4) The contents of Eclipse.org P2 repos changes; even for supposedly released versions.
      // The practical workaround to these Eclipse-related artifact publishing problems
      // involves building a complete Eclipse application that has all the needed plugins/features,
      // download that application and copy the needed plugins/features from its installation.

      val plugins = upd / "plugins"

      val jars =
        plugins ** "org.antlr.runtime_3.2.*.jar" +++
          plugins ** "org.eclipse.core.runtime_*.jar" +++
          plugins ** "org.eclipse.core.contenttype_*.jar" +++
          plugins ** "org.eclipse.core.jobs_*.jar" +++
          plugins ** "org.eclipse.core.resources_*.jar" +++
          plugins ** "org.eclipse.core.runtime_*.jar" +++
          plugins ** "org.eclipse.equinox.common_*.jar" +++
          plugins ** "org.eclipse.equinox.registry_*.jar" +++
          plugins ** "org.eclipse.emf.cdo.common_*.jar" +++
          plugins ** "org.eclipse.emf.cdo_*.jar" +++
          plugins ** "org.eclipse.emf.codegen.ecore.xtext_*.jar" +++
          plugins ** "org.eclipse.emf.codegen.ecore_*.jar" +++
          plugins ** "org.eclipse.emf.codegen_*.jar" +++
          plugins ** "org.eclipse.emf.common_*.jar" +++
          plugins ** "org.eclipse.emf.ecore.change_*.jar" +++
          plugins ** "org.eclipse.emf.ecore.xcore.lib_*.jar" +++
          plugins ** "org.eclipse.emf.ecore.xcore_*.jar" +++
          plugins ** "org.eclipse.emf.ecore.xmi_*.jar" +++
          plugins ** "org.eclipse.emf.ecore_*.jar" +++
          plugins ** "org.eclipse.emf.mwe.core_*.jar" +++
          plugins ** "org.eclipse.emf.mwe.util_*.jar" +++
          plugins ** "org.eclipse.emf.mwe2.language_*.jar" +++
          plugins ** "org.eclipse.emf.mwe2.launch_*.jar" +++
          plugins ** "org.eclipse.emf.mwe2.lib_*.jar" +++
          plugins ** "org.eclipse.emf.mwe2.runtime_*.jar" +++
          plugins ** "org.eclipse.net4j.util_*.jar" +++
          plugins ** "org.eclipse.osgi_*.jar" +++
          plugins ** "org.eclipse.xtemd.lib.macro_*.jar" +++
          plugins ** "org.eclipse.xtemd.lib_*.jar" +++
          plugins ** "org.eclipse.xtext.common.types*.jar" +++
          plugins ** "org.eclipse.xtext.ecore_*.jar" +++
          plugins ** "org.eclipse.xtext.util_*.jar" +++
          plugins ** "org.eclipse.xtext.xbase.lib_*.jar" +++
          plugins ** "org.eclipse.xtext.xbase_*.jar" +++
          plugins ** "org.eclipse.xtext.generator_*.jar" +++
          plugins ** "org.eclipse.xtext_*.jar"

      val jarFiles = jars.get.sortBy(_.name)

      s.log.info(s"jar files: ${jarFiles.size}")
      jarFiles.foreach { f =>
        s.log.info(s"=+ $f")
      }
      // Delete unused files.

      val all = (upd ** "*").get.filter(_.isFile).to[Set]
      val other = all -- jarFiles.to[Set]
      s.log.info(s"other: ${other.size}")
      other.foreach { f =>
        if (f.isFile)
          IO.delete(f)
        s.log.info(s"=> $f")
      }

      jars
    },

    owlapiLibs := {
      val s = streams.value
      val owlapiDir = baseDirectory.value / "target" / "owlapi"
      if (owlapiDir.exists()) {
        s.log.warn(s"*** Skip extracting to folder: $owlapiDir")
      } else {
        owlapiDir.mkdirs()
        for {
          c <- update.value.configurations
          if c.configuration == "compile"
          m <- c.modules
          (artifact, archive) <- m.artifacts
          if artifact.name.startsWith("imce.third_party.owlapi_libraries")
          if artifact.extension.contains("zip")
          _ = s.log.info(s"*** Artifact=$archive")
          files = IO.unzip(archive, owlapiDir)
        } yield ()
      }

      val jars = (owlapiDir ** "lib" * "*.jar").get.map(Attributed.blank)
      s.log.warn(s"=> Adding ${jars.size} unmanaged jars for the owlapi")

      jars
    },

    // Use when owlapi is not a sub-project.
    unmanagedJars in Compile := extractOMLProduct.value.classpath ++ owlapiLibs.value,
    unmanagedJars in (Compile, doc) := extractOMLProduct.value.classpath ++ owlapiLibs.value,

    // Use when owlapi is a subproject
//    unmanagedJars in Compile := extractOMLProduct.value.classpath,
//    unmanagedJars in (Compile, doc) := extractOMLProduct.value.classpath,

    fork in run := true,
    javaOptions in run ++= Seq(
      "-Dlog4j.debug=true",
      "-Dlog4j.configuration=log4j.properties")
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omf-scala-binding-owlapi",
    "gov.nasa.jpl.omf.scala.binding.owlapi",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.binding.owlapi"
        % Versions_omf_owlapi.version
        % "compile" withSources())
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omllFrameless",
    "gov.nasa.jpl.imce.oml.frameless",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.imce.oml.frameless"
        % Versions_oml_frameless.version
        % "compile" withSources())
  )