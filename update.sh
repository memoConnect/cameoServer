#!/bin/bash

set -e

case "$1" in
   "server")
	updateServer=true
      ;;
   "client")
	updateClient=true
      ;;
   "both")
	updateClient=true
	updateServer=true
      ;;
   *)
      echo -e "\e[33m[cameo - Update type required: [server|client|both] ]\033[0m"
      exit 1
      ;;
esac

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo -e "\e[33m[cameo - Changing to install dir: ${DIR}]\033[0m"
cd ${DIR}

fileName="UPDATING"
# check if we are alreade updating
if [ -e ${fileName} ]; then
    echo -e "\e[33m[cameo - another update is running, update scheduled]\033[0m"
    echo $1 > ${fileName}
    exit 0
else
    touch ${fileName}
fi

./stop.sh

# update secrets
if [ -d ../cameoSecrets ];then
    echo -e "\e[33m[cameo - updating secrets]\033[0m"
    cd ../cameoSecrets
    git pull || true
    cd -
else
    echo -e "\e[33m[cameo - directory cameoSecrets not found]\033[0m"
    exit 1
fi

# update server
if [ "$updateServer" = true ]; then
	echo -e "\e[33m[cameo - updating server]\033[0m"
	git pull
fi

#update client
if [ "$updateClient" = true ]; then
	if [ -d ../cameoJSClient ]; then
	    echo -e "\e[33m[cameo - updating client]\033[0m"
	    cd ../cameoJSClient
	    git pull
	    ./compile.sh
	    echo -e "\e[33m[cameo - copying client dist to public]\033[0m"
	    cp -r dist/* ../cameoServer/public/
	    cd - &> /dev/null
	else
	    echo -e "\e[33m[cameo - directory cameoJSClient not found]\033[0m"
	    exit 1
	fi
fi
./compile.sh
./start.sh ${mode}

# check if another update is sheduled
if [ -s ${fileName} ]; then
	nextMode=$(cat ${fileName})
    echo -e "\e[33m[cameo - found scheduled update, starting it now. Mode: ${nextMode}]\033[0m"
    rm ${fileName}
    ./update.sh ${nextMode}
else
    rm ${fileName}
fi
