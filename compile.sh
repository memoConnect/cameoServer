#!/bin/bash

# write version to file
if [ -z "$1" ]; then
    version=$1
else
    version="none"
fi

echo application.version=${version} > ./conf/version.conf

echo -e "\e[33m[cameo - Compiling version: $1 ]\033[0m"
./sbt clean compile stage
