#!/bin/bash

if [ -e ./target/universal/stage/RUNNING_PID ]; then
    pid=$(cat ${DIR}/target/universal/stage/RUNNING_PID)
    echo -e "\e[33m[cameo - stopped running app. PID: ${pid}]\033[0m"
    kill $pid
else
    echo -e "\e[33m[cameo - no running play app]\033[0m"
fi