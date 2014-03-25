#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo -e "\e[33m[cameo - Changing to install dir: ${DIR}]\033[0m"
cd ${DIR}

if [ -e ./target/universal/stage/RUNNING_PID ]; then
    pid=$(cat ./target/universal/stage/RUNNING_PID)
    echo -e "\e[33m[cameo - stopped running app. PID: ${pid}]\033[0m"
    kill $pid
else
    echo -e "\e[33m[cameo - no running play app]\033[0m"
fi