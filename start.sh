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

nohup bash -c "./target/universal/stage/bin/cameoserver $app_options" &