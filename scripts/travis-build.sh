#!/bin/bash

set -ev

# Build if TRAVIS_TAG is unset or empty.
[ -n "${TRAVIS_TAG}" ] && exit 0;

# Get the tag for this commit
t=$(git name-rev --tags --name-only "$(git rev-parse HEAD)")

# Bypass the build if the tag is anything but 'undefined'.
[ "undefined" != "$t" ] && exit 0;

git submodule init
git submodule update --checkout
export JVM_OPTS=@travis/jvmopts.compile
sbt -batch compile test
