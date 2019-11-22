# Converters among the canonical representations of OML models

## Copyrights

[Caltech](copyrights/Caltech.md)

## License

[Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0)

## Building & Downloads

- Building: [![Build Status](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.converters.svg?branch=master)](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.converters)
  

- Download:
    - Command-line application: [ ![Download](https://api.bintray.com/packages/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.imce.oml.converters/images/download.svg) ](https://bintray.com/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.imce.oml.converters/_latestVersion)
  
## Using OML Libraries

The OML Converter includes a self-contained application published as a tarball.

With SBT, add a dependency on [Coursier](http://get-coursier.io/).
This can be done in one of two ways:

### 1) Coursier as an SBT library dependency

In `project`, add a file: `coursier.sbt` with the following:
```sbt
libraryDependencies ++= Seq(
  "io.get-coursier" %% "coursier" % "1.0.0-RC10",
  "io.get-coursier" %% "coursier-cache" % "1.0.0-RC10"
)
```

### 2) Coursier as an SBT plugin dependency

In `project`, add a file: `coursier.sbt` with the following:
```sbt
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC10")
```

The two approaches have pros/cons:

| Coursier dependency | Pros/Cons | 
|---------------------|-----------|
| Library dependency (1) | + Does not affect the SBT project build |
|                        | - None of the Coursier functionality is available in the SBT shell |
| plugin dependency (2)  | - May interfere with the SBT project build |
|                        | + All the Coursier functionality is available in the SBT shell |

### Downloading & Installing OML Converter & libraries via SBT

```sbtshell

lazy val downloadOMLConverter = taskKey[PathFinder]("Download & install the OML Converter; if the OML libraries are needed for compilation, add `unmanagedJars in Compile := downloadOMLConverter.value.classpath`")

// Somwhere in a project settings context:

      downloadOMLConverter := {

        val omlDir: File = baseDirectory.value / "target" / "omlConverter"

        import scalaz.{\/,-\/,\/-}
        import scalaz.concurrent.Task

        val slog = streams.value.log

        val start = coursier.Resolution(
          Set(
            coursier.Dependency(
              coursier.Module("gov.nasa.jpl.imce", "gov.nasa.jpl.imce.oml.converters_2.11"),
              "0.1.2.0"
            )
          )
        )

        val repositories = Seq(
          coursier.MavenRepository("https://dl.bintray.com/jpl-imce/gov.nasa.jpl.imce")
        )

        val fetch = coursier.Fetch.from(repositories, coursier.Cache.fetch())

        val resolution = start.process.run(fetch).unsafePerformSync

        val localArtifacts: Seq[coursier.FileError \/ File] = Task.gatherUnordered(
          resolution
            .classifiersArtifacts(Seq("resource"))
            .flatMap {
              case a if
              a.url.contains("gov.nasa.jpl.imce.oml.converters") &&
                a.url.endsWith("-resource.tgz") =>
                Some(coursier.Cache.file(a).run)
              case a =>
                None
            }
        ).unsafePerformSync

        localArtifacts.foreach {
          case -\/(fileError) =>
            slog.error(fileError.describe)
          case \/-(file) =>
            if (!omlDir.exists) {
              omlDir.mkdirs()
              slog.info(s"Installing OML Converter from local artifact: $file")
              s"tar --strip-components 1 -C $omlDir -zxvf $file" !
            }

        }

        val jars = omlDir / "lib" ** "*.jar"

        jars
      }
```
## Description

OML supports three canonical representations:
1) OML textual concrete syntax (`*.oml`)

   The [OML Xtext grammar](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.oml.tycho/blob/master/plugins/core/gov.nasa.jpl.imce.oml.dsl/src/gov/nasa/jpl/imce/oml/dsl/OML.xtext)
   specifies the grammar of this textual concrete syntax.
  
   The [OML Workbench](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.oml.tycho/tree/master/releng/gov.nasa.jpl.imce.oml.product) 
   is an Eclipse-based RCP application with support for editing OML models in this textual concrete syntax.
   
   Thanks to [Eclipse Xtext](https://www.eclipse.org/Xtext/), there is a seamless conversion between the
   OML textual representation and an equivalent [Eclipse EMF](https://www.eclipse.org/modeling/emf/) object-oriented representation.
   
2) [OML normalized tabular relational schema](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.oml.tables) (`*.omlzip`)

   This normalized relational schema is designed for the following objectives:
   - Support high-performance, distributed data analysis of OML models on [Apache Spark](http://spark.apache.org/) clusters.
   
   - Ensure that any OML model has a unique serialization as a sorted OML normalized tabular relational schema.
   
   - Ensure that any two OML models are equivalent if and only if 
     their sorted OML normalized tabular relational schema serializations are equivalent.

3) [OML ontology in a restricted subset of OWL2-DL + SWRL](https://github.com/JPL-IMCE/gov.nasa.jpl.omf.scala.binding.owlapi) (`*.owl`)

   The restricted subset of [OWL2-DL](https://www.w3.org/TR/owl2-syntax/) with [SWRL](https://www.w3.org/Submission/SWRL/)
   was crafted over several years of working on integrating descriptive modeling at JPL.
   
   See:
   - [OMG SEDSIG Reston, Mar 2012](http://syseng.omg.org/syseng_info.htm#Reston-meeting-2012)
   - [OMG SEDSIG Santa Clara, Dec 2011](http://syseng.omg.org/syseng_info.htm#SantaClara-meeting-2011)
   - [OMG SEDSIG Arlington, Mar 2011](http://syseng.omg.org/syseng_info.htm#Arlington-meeting-2011)
   - [OMG SEDSIG Cambridge, Sep 2010](http://syseng.omg.org/syseng_info.htm#Boston-meeting-2010)
   
## Usage:

- Get the OML Converter command-line application: [ ![Download](https://api.bintray.com/packages/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.imce.oml.converters/images/download.svg) ](https://bintray.com/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.imce.oml.converters/_latestVersion)
           
  ```bash
  tar zxf gov.nasa.jpl.imce.oml.converters_2.11-<version>-resource.tgz
  ```
  
  This tarball contains a folder, `OMLConverter` with two sub-folders:
  - `OMLConverter/bin`: platform-specific scripts to launch the command-line application on Linux, MacOSX and Windows.
  - `OMLConverter/lib`: shared libraries used by the directory converter application.

- Print OML Converter usage information

  ```
  omlConverter --help
  ```
  
  Produces:
  
  ```
    BuildInfo name: gov.nasa.jpl.imce.oml.converters, version: 0.12-ed864e66ee04f0849ab7d2a77cc349ba1f43e7f6-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.18, buildDateUTC: 2019-11-22-00:33
    Usage: omlConverter [text|owl|json|parquet|sql|diff|merge] [options] <args>...
    
    Command: text [options] [<oml.catalog.xml>]
      Converts all input OML textual syntax files ('*.oml' and/or '*.omlzip') found in scope of an `oml.catalog.xml`
    
      [options]:
      --resolve:all            Invoke EcoreUtils.resolveAll() before conversion
    
      required arguments:
      <oml.catalog.xml>        An OASIS XML Catalog file named 'oml.catalog.xml' to search for '*.oml' and '*.oml.zip' files.
    
    
    Command: owl [<oml.catalog.xml>]
      Converts all input OML files in OWL2-DL + SWRL rules ('*.owl') found in scope of an `oml.catalog.xml`
    
      required arguments:
      <oml.catalog.xml>        An OASIS XML Catalog file named 'oml.catalog.xml' to search for '*.owl' files.
    
    
    Command: json [<oml.catalog.xml>]
      Converts all input OML tabular json archive files ('*.omlzip') found in scope of an `oml.catalog.xml`
    
      required arguments:
      <oml.catalog.xml>        An OASIS XML Catalog file named 'oml.catalog.xml' to search for '*.omlzip' files.
    
    
    Command: parquet <oml.parquet folder>
      Convert from folders of OML parquet table files, '<dir>/<oml table>.parquet'.
    
      required arguments:
      <oml.parquet folder>     A folder of OML parquet table files: '<dir>/<oml table>.parquet'.
    
    
    Command: sql <server>
      Convert from an SQL server.
    
      required arguments:
      <server>                 SQL server
    
    Command: diff <dir1> <dir2>
      Compare OML files recursively between two directories.
    
      required arguments:
      <dir1>                   Left side comparison, <dir1>.
      <dir2>                   Right side comparison, <dir2>.
    
    Command: merge <oml.parquet folder>...
      Merge OML content from multiple 'oml.parquet' folders.
      To merge OML content from different representations (e.g., text, owl, json, ...),
      first convert each representation to parquet, then merge the resulting 'oml.parquet' folders.
    
      required arguments:
      <oml.parquet folder>...  One or more paths to 'oml.parquet' folders to include in the merge.
    
    General Options:
    
      --version                Shows build information.
    
      --help                   Prints usage information about the OML Converter.
                               For help about launcher arguments, try: -h.
                               To specify system properties, use:
                                 -Dkey1=val1 ... -DkeyN=valN -- <OML Converter command & option arguments>
                               Example for Apache SPARK properties:
                                 -Dspark.app.name=MyConversion -Dspark.master=local[8] ... -- ...
                               Example for Apache SPARK monitoring:
                                 -Dspark.eventLog.enabled=true -Dspark.eventLog.dir=file:///tmp/spark-events ... -- ...
                               Note: current directory:
                               /opt/local/imce/github.jpl-imce/gov.nasa.jpl.imce.oml.converters
    
    
      -v:files, --verbose:files
                               Verbose: show the input files found for each 'rewriteURI' entry of an OML Catalog.
    
      -out:modules, --output:modules <value>
                               For conversion to OWL, writes a YAML file with a IRI to file location mapping for each converted OML module.
                               For other conversions, writes a text file with the list of IRIs of all OML modules.
                               (Not applicable for 'diff' command).
    
      -out:cat, --output:catalog <value>
                               Output catalog where to write conversion results.
                               (Cannot be used with --output or -out).
    
      -out, --output <value>   Output folder where to write conversion results.
                               (Cannot be used with --output:catalog or -out:cat).
    
      -out:fuseki, --output:fuseki <value>
                               Output to a fuseki server where <value> is a fuseki server url of the form:
                               http://<hostname>:<port>/<datasetname> (i.e. without the '/data' suffix)
                               Requires output conversion with --owl or -o.
                               Requires the Jena Fuseki command 's-put' to be available on the path.
                               Requires the Jena Fuseki dataset to exist.
                               (Not applicable for 'diff' command).
    
      --clear                  Make sure the output folder is deleted (if it exists) and is created empty before writing conversion results.
    
      --hsort                  Disable hierarchically sort the hypergraph of OML Modules before output conversion (default = enabled)
    
      -t, --text               Output conversion includes OML as textual syntax '*.oml' files
                               (Not applicable for 'merge' command).
    
      -o, --owl                Output conversion includes OWL2-DL + SWRL rules '*.owl' ontology files, one for each OML module.
                               (Not applicable for 'merge' command).
    
      -j, --json               Output conversion includes archive files, '*.omlzip' of OML json tables, one for each OML module.
                               (Not applicable for 'merge' command).
    
      -p:each, --parquet:each  Output conversion includes 'oml.parquet' output folders, one for each OML module.
                               (Caution: this is slow!)
                               Note: Ignore warnings from Apache Spark like this for some <N> and <S>:
                               WARN TaskSetManager: Stage <N> contains a task of very large size (<S> KB). The maximum recommended task size is 100 KB.
    
      -p, --parquet            Output conversion aggregates all OML modules into a single 'oml.parquet' output folder.
                               Note: Ignore warnings from Apache Spark like this for some <N> and <S>:
                               WARN TaskSetManager: Stage <N> contains a task of very large size (<S> KB). The maximum recommended task size is 100 KB.
    
      -s, --sql <value>        Output conversion aggregates all OML modules into OML data stored on an SQL server.
    
    
    Build information
    name: gov.nasa.jpl.imce.oml.converters, version: 0.12-ed864e66ee04f0849ab7d2a77cc349ba1f43e7f6-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.18, buildDateUTC: 2019-11-22-00:33
    
      --excludeOMLImports      Excludes OML imports in OWL output.
    
      --excludeOMLContents     Excludes OML imports and content in OWL output.
    
      --excludePurlImports     Excludes purl.org imports in OWL output.
          
  ```
