# Converters among the canonical representations of OML models

[![Build Status](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.converters.svg?branch=master)](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.converters)

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

- Download the `OMLConverter` command-line application (compatible with Linux, MacOSX, Windows)

- `omlConverter`

    This will show command line usage information about three different modes,
    one for converting OML models from one of each canonical representations to the other two.
    
### Convert from OML textual concrete Syntax

- `omlConverter -cat <oml.catalog.xml> <*.oml> ...`
    
    Use the `oml.catalog.xml` file to convert all `*.oml` files from the OML textual concrete syntax representation 
    to corresponding OML ontological (`*.owl`) and tabular representations (`*.oml.json.zip`).
    
### Convert from OML normalilzed tabular relational schema

- `omlConverter -cat <oml.catalog.xml> <*.oml.json.zip> ....`
    
   Use the `oml.catalog.xml` file to convert all `*.oml.json.zip` files from the OML tabular representation 
   to corresponding OML ontological (`*.owl`) and textual concrete syntax representations (`*.oml`).
       
    
### Convert from OML ontologies

- `omlConverter -cat <oml.catalog.xml> <IRI> ...`
                     
  Use the `oml.catalog.xml` file to convert all OML ontological representations resolved from the `<IRI>` provided
  to corresponding OML textual concrete syntax (`*.oml`) and tabular representations (`*.oml.json.zip`).
      
- `omlConverter -cat <oml.catalog.xml> <*.owl> ...`
                     
  Use the `oml.catalog.xml` file to convert all `*.owl` files from the OML ontological representation
  to corresponding OML textual concrete syntax (`*.oml`) and tabular representations (`*.oml.json.zip`).
      