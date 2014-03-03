#!/bin/bash
for spec in $(ls test); do
    if [ -f "test/$spec" ] ; then
        specName=${spec%.*}
        echo "Testing: " ${specName}
        ./sbt "test-only ${specName} html junitxml console"
    fi
done



