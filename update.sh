#!/bin/bash

fileName="UPDATING"
# check if we are alreade updating
if [ -e ${fileName} ]; then
    echo -e "\e[33m[cameo - another update is running, update scheduled]\033[0m"
    echo "+1" > ${fileName}
    exit 0
else
    touch ${fileName}
fi

./stop.sh

# update secrets
if [ -d ../cameoSecrets ];then
    echo -e "\e[33m[cameo - updating secrets]\033[0m"
    cd ../cameoSecrets
    git pull
    cd -
else
    echo -e "\e[33m[cameo - directory cameoSecrets not found]\033[0m"
    exit 1
fi

# update server
echo -e "\e[33m[cameo - updating server]\033[0m"
git pull

#update client
if [ -d ../cameoJSClient ]; then
    echo -e "\e[33m[cameo - updating client]\033[0m"
    cd ../cameoJSClient
    git pull
    ./compile.sh
    cd -
else
    echo -e "\e[33m[cameo - directory cameoJSClient not found]\033[0m"
    exit 1
fi

./compile.sh
./start.sh ${mode}

# check if another update is sheduled
if [ -s ${fileName} ]; then
    echo -e "\e[33m[cameo - found scheduled update, starting it now]\033[0m"
    rm ${fileName}
    ./update.sh
else
    rm ${fileName}
fi
