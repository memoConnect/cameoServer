#!/bin/bash

if [ -z "${1}" ]; then
    _sbtTarget="run"
else
    _sbtTarget="atmos:run"
fi

./updateSource.sh
./compile.sh
./sbt ${_sbtTarget}
