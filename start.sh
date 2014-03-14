#!/bin/bash

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
      app_options=-Dconfig.file=/opt/cameoSecrets/secret_prod.conf
      ;;
   "dev")
       echo -e "\e[33m[cameo - Running as dev]\033[0m"
      app_options=-Dconfig.file=/opt/cameoSecrets/secret_dev.conf
      ;;
   "local")
      echo -e "\e[33m[cameo - Running as local]\033[0m"
      app_options=-Dconfig.file=/opt/cameoSecrets/secret_local.conf
      ;;
   *)
      echo Invalid deployment type: ${mode}
      exit 1
      ;;
esac

nohup bash -c "./target/universal/stage/bin/cameoserver $app_options > /dev/null" & > /dev/null