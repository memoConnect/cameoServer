#!/bin/bash

set -e

 # change to install dir
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo -e "\e[33m[cameo - Changing to install dir: ${DIR}]\033[0m"
cd ${DIR}

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
   "none")
    ;;
   *)
      echo -e "\e[33m[cameo - Update type required: [server|client|both|none] ]\033[0m"
      exit 1
      ;;
esac

# get app mode
if [ -e "mode" ]; then
    mode=$(cat mode)
else
    echo -e "\e[33m[cameo - mode file not found. exiting]\033[0m"
    exit 1
fi

case "$mode" in
   "prod")
      echo -e "\e[33m[cameo - Running as prod]\033[0m"
      appOptions=-Dconfig.file=/opt/cameoSecrets/secret_prod.conf
      ;;
   "stage")
      echo -e "\e[33m[cameo - Running as stage]\033[0m"
      appOptions=-Dconfig.file=/opt/cameoSecrets/secret_stage.conf
      ;;
   "dev")
       echo -e "\e[33m[cameo - Running as dev]\033[0m"
      appOptions=-Dconfig.file=/opt/cameoSecrets/secret_dev.conf
      ;;
   "local")
      echo -e "\e[33m[cameo - Running as local]\033[0m"
      appOptions=-Dconfig.file=/opt/cameoSecrets/secret_local.conf
      ;;
   *)
      echo "\e[33m[cameo - Invalid mode: ${mode}]\033[0m"
      exit 1
      ;;
esac

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
	if [ "$mode" == "stage" ]; then
	    git fetch --tags
        git checkout tags/stage
    else
	    git pull
	fi
fi

#update client
if [ "$updateClient" = true ]; then
	if [ -d ../cameoJSClient ]; then
	    echo -e "\e[33m[cameo - updating client]\033[0m"
	    cd ../cameoJSClient
	    if [ "$mode" == "stage" ]; then
	        git fetch --tags
            git checkout tags/stage
        else
            git pull
        fi
	    ./compile.sh #todo pass stage
	    echo -e "\e[33m[cameo - copying client dist to public]\033[0m"
        mkdir -p ../cameoServer/public
	    cp -r dist/* ../cameoServer/public/
	    cd - &> /dev/null
	else
	    echo -e "\e[33m[cameo - directory cameoJSClient not found]\033[0m"
	    exit 1
	fi
fi

./compile.sh
./start.sh ${appOptions}

# check if another update is sheduled
if [ -s ${fileName} ]; then
	nextMode=$(cat ${fileName})
    echo -e "\e[33m[cameo - found scheduled update, starting it now. Mode: ${nextMode}]\033[0m"
    rm ${fileName}
    ./update.sh ${nextMode} $2
else
    rm ${fileName}
fi
