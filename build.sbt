
import sbt.Keys._
import sbt._

import java.lang.System
import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

updateOptions := updateOptions.value.withCachedResolution(true)

import scala.io.Source
import scala.util.control.Exception._

resolvers += "Xtext-core 2.11 maintenance" at "http://services.typefox.io/open-source/jenkins/job/xtext-core/job/maintenance_2.11/lastSuccessfulBuild/artifact/build/maven-repository"

resolvers += "Xtext-xtend 2.11 maintenance" at "http://services.typefox.io/open-source/jenkins/job/xtext-xtend/job/maintenance_2.11/lastSuccessfulBuild/artifact/build/maven-repository"

resolvers += "Xtext-lib 2.11 maintenance" at "http://services.typefox.io/open-source/jenkins/job/xtext-lib/job/maintenance_2.11/lastSuccessfulBuild/artifact/build/maven-repository"

resolvers += "Xtext-eclipse 2.11 maintenance" at "http://services.typefox.io/open-source/jenkins/job/xtext-eclipse/job/maintenance_2.11/lastSuccessfulBuild/artifact/build/maven-repository"

val EclipseNeonRepo = "http://builds.gradle.org:8000/eclipse/update-site/mirror/releases-neon"

def eclipsePlugin(name: String, version: String)
: ModuleID
= ModuleID(organization="plugins", name=name, revision=version)
  .from(s"${EclipseNeonRepo}/plugins/${name}_${version}.jar")

lazy val core = Project("omlConverters", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(JavaAppPackaging)
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2017",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    mainClass in Compile := Some("gov.nasa.jpl.imce.oml2omf.OML2OMF"),

    buildInfoPackage := "gov.nasa.jpl.imce.oml2omf",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,
    // include all test artifacts
    publishArtifact in Test := true,

    scalaSource in Test := baseDirectory.value / "test",

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),

    resolvers += MavenCache( "gradle mavenized eclipse target platform",
      file(System.getenv("HOME")+"/.tooling/eclipse/targetPlatforms/46/mavenized-target-platform/")),

    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    scalacOptions in (Compile, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Test, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Compile, doc) += "-Xplugin-disable:artima-supersafe",
    scalacOptions in (Test, doc) += "-Xplugin-disable:artima-supersafe",

    scalacOptions += "-g:vars",

    libraryDependencies ++= Seq(
      "gov.nasa.jpl.imce.oml" % "gov.nasa.jpl.imce.oml.dsl" % Versions_oml_core.version exclude("org.apache", "org.apache.log4j"),

      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.imce.oml.resolver" % Versions_oml_resolver.version,

      "gov.nasa.jpl.imce" %% "imce.third_party.other_scala_libraries"
        % Versions_other_scala_libraries.version artifacts
        Artifact("imce.third_party.other_scala_libraries", "zip", "zip", "resource"),

      "org.eclipse.xtext" % "org.eclipse.xtext" % "2.11.1-SNAPSHOT",
      eclipsePlugin("org.eclipse.emf.mwe2.runtime", "2.9.0.v201605261103"),
      eclipsePlugin("org.eclipse.uml2.uml", "5.2.3.v20170227-0935"),
      eclipsePlugin("org.eclipse.uml2.common", "2.1.0.v20170227-0935"),
      eclipsePlugin("org.eclipse.uml2.uml.profile.standard", "1.0.100.v20170227-0935"),
      eclipsePlugin("org.eclipse.uml2.types", "2.0.0.v20170227-0935"),
      eclipsePlugin("org.eclipse.uml2.uml.resources", "5.2.0.v20170227-0935")
    )
  )
//  .dependsOnSourceProjectOrLibraryArtifacts(
//    "jpl.omf.schema.resolver",
//    "jpl.omf.schema.resolver",
//    Seq(
//      "gov.nasa.jpl.imce" %% "jpl.omf.schema.resolver"
//        % Versions_omf_schema_resolver.version
//        % "compile" artifacts(
//        Artifact("jpl.omf.schema.resolver"),
//        Artifact("jpl.omf.schema.resolver", "zip", "zip", "resource")
//        )
//    )
//  )
