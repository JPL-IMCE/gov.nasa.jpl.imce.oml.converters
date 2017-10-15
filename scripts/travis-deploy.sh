#!/bin/bash

set -ev

# Deploy if TRAVIS_TAG is set.
# Error if TRAVIS_SECURE_ENV_VARS is false

[ -z "${TRAVIS_TAG}" ] && exit 0;
[ ! ${TRAVIS_SECURE_ENV_VARS} ] && exit -1;

. $(dirname $0)/travis-decode.sh

chmod 600 local.*
eval `ssh-agent -s`
ssh-add local.deploy_key
git config --global push.default simple
git config --global user.email "nobody@nobody.org"
git config --global user.name "Travis CI"

sbt -jvm-opts travis/jvmopts.compile -Dproject.version=$TRAVIS_TAG publishSigned universal:publish ghpagesPushSite

