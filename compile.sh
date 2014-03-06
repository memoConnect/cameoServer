#!/bin/bash
echo "compile the project"

branch=$(git symbolic-ref --short HEAD)
if [ "$branch" -eq "dev" ]; then
    # get latest successfull build (if on dev)
    build=$(git describe --abbrev=0 --tags | cut -d"_" -f2)


./sbt clean compile stage