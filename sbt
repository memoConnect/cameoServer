java -Xms512M -Xmx1024M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -javaagent:./newrelic/newrelic.jar -jar `dirname $0`/tools/sbt-launch.jar "$@"
