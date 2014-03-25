#!/bin/bash

# get app mode
if [ -e "mode" ]; then
    mode=$(cat mode)
else
    echo -e "\e[33m[cameo - mode file not found. exiting]\033[0m"
    exit 1
fi

branch=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD)
case "$mode" in
   "prod")
      echo -e "\e[33m[cameo - Compiling as prod]\033[0m"
      version="moep"
      ;;
   "stage")
      echo -e "\e[33m[cameo - Compiling as stage]\033[0m"
      # get version number from last tag
      tag=$(git describe --abbrev=0 --tags)
      version=${tag}_stage
      ;;
   "dev")
      echo -e "\e[33m[cameo - Compiling as dev]\033[0m"
      # get latest successfull build from tag
      buildNum=$(git describe --abbrev=0 --tags | cut -d"_" -f2)
      version=build.${buildNum}
      ;;
   "local")
      echo -e "\e[33m[cameo - Compiling as local]\033[0m"
      version=branch.${branch}
      ;;
   *)
      echo -e "\e[33m[cameo - Invalid mode: ${mode}]\033[0m"
      exit 1
      ;;
esac

# write version to file
echo application.version=${version} > ./conf/version.conf

echo -e "\e[33m[cameo - Compiling version: ${version}]\033[0m"
./sbt clean compile stage
