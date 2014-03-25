#!/bin/bash

# stop the app fist ;)
./stop.sh

nohup bash -c "./target/universal/stage/bin/cameoserver "$@" > /dev/null" &> /dev/null &
