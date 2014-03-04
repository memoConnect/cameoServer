cameoServer
================

cameo - The secure messenger hub with social elements

cameo aims to be the new open multimedia messenger and social platform hub, that connects to the existing platforms - like jabber, SMS, email, Skype, faceboook, … -  and it is open soucre.

cameo integrates all your important messenger and social services into one new universe, that is still connected to the existing outside communication islands.

cameo is:

* end-to-end encrypted (RAS4096, AES256)
* easy to use
* enables users to reach all of their friends, does

Furthermore it is a European/German start up…

Join this new success story …

cameo Client
================

[here](https://github.com/memoConnect/cameoJSClient) you can find cameo web client

Technical issues
================

* Playframework2
* Scala
* Akka
* mongodb
* OpenStack
* AngularJS
    * Phonegap
    * Bootstrap

API
===

cameo Server offers a API. API docs can be found [here](http://docs.cameo.apiary.io/)

Getting started
================

1. git clone git@github.com:memoConnect/cameoServer.git
1. cd cameoServer
1. ./cleanRun.sh
1. open in browser http://localhost:9000

Using Typesafe Console (dev only)
=================================

1. git clone git@github.com:memoConnect/cameoServer.git (or pull if you have already a clone on your disk)
1. cd cameoServer
1. ./cleanRun.sh console

App: <http://localhost:9000/>

Console: <http://localhost:9900/>

Test to get some actors working, using testdata in dev mode:

    curl -X POST -H "Content-Type: application/json" -H "Authorization:viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ" -d '{"messageBody": "text"}' http://localhost:9000/api/v1/conversation/OM9QeJ4RfJcdscyo52g4/message

More doku about it here at [Typesafe](http://typesafe.com/platform/runtime/console)

LICENSE
================

cameo source files are made available under the terms of the GNU Affero General Public License (AGPL). See individual files for details.
