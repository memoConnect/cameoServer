#!/bin/bash
echo -e "\e[33m[cameo - compiling the app]\033[0m"

branch=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD)
case $branch in
    "dev")
        # get latest successfull build from tag
        buildNum=$(git describe --abbrev=0 --tags | cut -d"_" -f2)
        version=dev_build.${buildNum}
        ;;
    "master")
        version="ToDo"
        ;;
    *)
        version=${branch}
        ;;
esac

# write version to file
echo application.version=${version} > ./conf/version.conf

./sbt clean compile stage
