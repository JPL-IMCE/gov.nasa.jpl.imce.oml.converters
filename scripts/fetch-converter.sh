#!/bin/bash

# Get coursier: https://github.com/coursier/coursier#command-line

# Change as needed.
export R=cae-artifactory.jpl.nasa.gov/artifactory/maven-libs-release-local

echo -n "Username for $R: "
read U

echo -n "Password (does not work if it includes characters like '/', '#', '?'): "
read -s P

echo
echo "Fetching..."
coursier fetch -C resource -A tgz -A zip \
    -r "https://$U:$P@$R" \
    -r bintray:jpl-imce/gov.nasa.jpl.imce \
    gov.nasa.jpl.imce:gov.nasa.jpl.imce.oml.converters_2.11:0.1.0.2

echo "Done"