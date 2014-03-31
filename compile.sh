#!/bin/bash

# write version to file
if [ -z "$1" ]; then
    version="none"
else
    version=$1
fi

echo application.version=${version} > ./conf/version.conf

echo -e "\e[33m[ CameoServer - Compiling version: ${version} ]\033[0m"

if [ "$2" == "quick" ]; then
    ./sbt compile stage
else
    ./sbt clean compile stage
fi
