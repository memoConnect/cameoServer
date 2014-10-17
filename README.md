cameoServer
================

The backend for the [cameoNet](https://cameonet.de) messenger. It is developed in Scala using the [Play Framework](https://www.playframework.com/).

The [web client](https://github.com/memoConnect/cameoJSClient) can be optionally installed to /public. See the client repository for installation instructions.


API Documentation
----------------

The documentation of the API can be found [here](http://docs.cameo.apiary.io/) 


Dependencies
----------------

* Java 7


Run locally for testing and development
----------------

* run './sbt run' in the project folder
* a mongoDB instance with some test data will be started in memory
* all dependencies will be downloaded and the instance will listen on port 9000

* the tests can be run with "./sbt test"


Deploy on server
----------------

* A mongoDB v2.6.3 instance is required
* Adjust configuration in "conf/application_prod.conf"
* run './sbt "start -Dconfig.file=conf/application_prod.conf"' in the project folder
* all dependencies will be downloaded and the instance will listen on port 9000
* press "Ctrl-D" to keep the server running in the background


Licence
----------------

cameoNet source files are made available under the terms of the GNU Affero General Public License (AGPL).


Third Party Libraries and Frameworks
----------------

* [play framework](https://www.playframework.com/) - Licence: Apache 2 
* [mongodb]( http://www.mongodb.org/) - Licence: AGPL v3.0 
* [reactivemongo](http://reactivemongo.org/) - Licence: Apache 2
* [jBCrypt](http://www.mindrot.org/projects/jBCrypt/) - Licence: BSD  
* [libphonenumber](https://code.google.com/p/libphonenumber/) - Licence: Apache 2
* [batik](https://xmlgraphics.apache.org/batik/) - Licence: Apache 2
* [imgscalr](http://www.thebuzzmedia.com/software/imgscalr-java-image-scaling-library/) - Licence: Apache 2