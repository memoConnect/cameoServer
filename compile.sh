#!/bin/bash

# write version to file
if [ -z "$1" ]; then
    version="none"
else
    version=$1
fi

echo application.version=${version} > ./conf/version.conf

echo -e "\e[33m[cameo - Compiling version: ${version} ]\033[0m"
./sbt clean compile stage
