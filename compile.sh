#!/bin/bash

# write version to file
echo application.version=$1 > ./conf/version.conf

echo -e "\e[33m[cameo - Compiling version: ${version}]\033[0m"
./sbt clean compile stage
