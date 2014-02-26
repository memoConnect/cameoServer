#!/bin/bash

case "$1" in
   "prod")
      echo Running as prod
      app_options=-Dconfig.file=/opt/cameoSecrets/secret_prod.conf
      ;;
   "dev")
      echo Running as dev
      app_options=-Dconfig.file=/opt/cameoSecrets/secret_dev.conf
      ;;
   "local")
      echo Running as local
      ;;
   *)
      echo Deployment type is required: "[prod|dev|local]"
      exit 1
      ;;
esac

case "$2" in
   "server")
      ;;
   "client")
      ;;
   *)
      echo Deployment component is required: "[server|client]"
      exit 1
      ;;
esac

fileName="UPDATING"
# check if we are alreade updating
if [ -e ${fileName} ]; then
    echo "already updating, update scheduled"
    echo $2 > ${fileName}
    exit 1
else
    touch ${fileName}
fi

./stop.sh

if [ "$2" == "client" ];then
    cd public
    git pull
    cd ..
else
    git pull
    if [ -d ../cameoSecrets ]
        echo "updating cameoSecrets"
        cd ../cameoSecrets
        git pull
        cd -
    fi
fi

./compile.sh
./start.sh $1

# check if another update is sheduled
if [ -f ${fileName} ]; then
    c=$(cat ${fileName})
    echo "found scheduled update, starting it now"
    rm ${fileName}
    ./update.sh $1 ${c} &
else
    rm ${fileName}
fi
