# Converters among the canonical representations of OML models

## Copyrights

[Caltech](copyrights/Caltech.md)

## License

[Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0)

## Building & Publishing

- Building: [![Build Status](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.converters.svg?branch=master)](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.converters)
  (Note: Travis-CI builds are currently failing because the [OML Workbench](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.oml.tycho) is too large to publish on Bintray)


- Publishing:
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
   
2) [OML normalized tabular relational schema](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.oml.tables) (`*.oml.json.zip`)

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

- Download the [IMCE Ontologies Processor docker image](https://hub.docker.com/r/jplimce/gov.nasa.jpl.imce.ontologies.processor/tags/)


  ```
  docker pull jplimce/gov.nasa.jpl.imce.ontologies.processor:<version>
  ```
  
  This image includes the `omlConverter` as `/imce/tools/

- Run the image with a local folder that is the root of an OML ontology tree mapped as `/imce/ontologies`

  ```
  docker run -t -v `pwd`:/imce/ontologies -i jplimce/gov.nasa.jpl.imce.ontologies.processor:<version> /bin/bash
  ```

- Inside the image, the `omlConverter` is available as: `/imce/tools/omlConverter/bin/omlConverter`

- `omlConverter`

    This will show command line usage information about three different modes,
    one for converting OML models from one of each canonical representations to the other two.
    
### Convert from OML textual concrete Syntax

- `omlConverter -cat <oml.catalog.xml> [-out <oml.metadata.json>] <*.oml> ...`
    
    Use the `oml.catalog.xml` file to convert all `*.oml` files from the OML textual concrete syntax representation 
    to corresponding OML ontological (`*.owl`) and tabular representations (`*.oml.json.zip`).
    
    If specified, save the OML Metadata Directed Graph to `<oml.metadata.json>`
    
### Convert from OML normalilzed tabular relational schema

- `omlConverter -cat <oml.catalog.xml> [-out <oml.metadata.json>] <*.oml.json.zip> ....`
    
   Use the `oml.catalog.xml` file to convert all `*.oml.json.zip` files from the OML tabular representation 
   to corresponding OML ontological (`*.owl`) and textual concrete syntax representations (`*.oml`).
    
   If specified, save the OML Metadata Directed Graph to `<oml.metadata.json>`
    
### Convert from OML ontologies

- `omlConverter -cat <oml.catalog.xml> [-out <oml.metadata.json>] <IRI> ...`
                     
  Use the `oml.catalog.xml` file to convert all OML ontological representations resolved from the `<IRI>` provided
  to corresponding OML textual concrete syntax (`*.oml`) and tabular representations (`*.oml.json.zip`).
      
  If specified, save the OML Metadata Directed Graph to `<oml.metadata.json>`
    
- `omlConverter -cat <oml.catalog.xml> [-out <oml.metadata.json>] <*.owl> ...`
                     
  Use the `oml.catalog.xml` file to convert all `*.owl` files from the OML ontological representation
  to corresponding OML textual concrete syntax (`*.oml`) and tabular representations (`*.oml.json.zip`).
      
  If specified, save the OML Metadata Directed Graph to `<oml.metadata.json>`
  
### OML Metadata Directed Graph

Each graph node represents an OML Module that has been converted.
Each directed graph edge corresponds to an OML ModuleEdge from an importing OML Module to an imported OML Module.

Synopsis of an [OML Metadata Directed Graph](src/main/scala/gov/nasa/jpl/imce/oml/converters/metadata/OMLMetadataGraph.scala):

```json
{
  "nodes": [<OML Converted Module>*],
  "edges": [<OML Converted Module Edge>*]
}
```

Synopsis of an [OML Converted Module](src/main/scala/gov/nasa/jpl/imce/oml/converters/metadata/OMLConvertedModule.scala):

```json
{
  "iri" : "<OML Module IRI>",
  "filename" : "<relative pathname from the directory location of the `oml.catalog.xml` file>",
  "provenance" : "<OMLMetadataProvenance>"
}
```

where [OMLMetadataProvenance](src/main/scala/gov/nasa/jpl/imce/oml/converters/metadata/OMLMetadataProvenance.scala) is be one of the following:

- OMLBuiltinModuleProvenance

    An OMLConvertedModule with OMLBuiltinModuleProvenance
    corresponds to an OML TerminologyGraph ontology that defines either datatypes
    that are part of the OWL2-DL datatype map or annotation properties
    that are used in defining the OWL2-DL datatype map or the OWL2 vocabulary.
    
- OMLBundleModuleProvenance

    An OMLConvertedModule with OMLBundleModuleProvenance
    corresponds to an OML TerminologyBox
    that is either an OML Bundle or an OML TerminologyBox that
    is directly or indirectly imported by an OML Bundle.
    
- OMLExtensionModuleProvenance
    
    An OMLConvertedModule with OMLExtensionModuleProvenance
    corresponds to an OML Module that directly or indirectly
    imports an OML Bundle and is not directly or indirectly imported by an OML Bundle.
    
- OMLOtherModuleProvenance

    An OMLConvertedModule with OMLOtherModuleProvenance
    corresponds to an OML Module that is not OMLBuiltinModuleProvenance,
    OMLBundleModuleProvenance or OMLExtensionModuleProvenance.

Synopsis of an [OML Converted ModuleEdge](src/main/scala/gov/nasa/jpl/imce/oml/converters/metadata/OMLConvertedModuleEdge.scala):

```json
{
  "importing" : "<IRI of the importing OML Converted Module>",
  "imported" : "<IRI of the imported OML Converted Module>"
}
```