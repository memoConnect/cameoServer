require.config({
    baseUrl: "js"
    // alias libraries paths
   ,urlArgs: "bust=" + (new Date()).getTime()
   ,paths: {
        'angular': 'vendor/angular/angular.min'
       ,'angular-route': 'vendor/angular/angular-route.min'
       ,'angular-resource': 'vendor/angular/angular-resource.min'
       ,'angular-cookies': 'vendor/angular/angular-cookies.min'

       ,'angularAMD': 'vendor/requirejs/angularAMD'
       ,'ngload': 'vendor/requirejs/ngload'

       ,'LoginCtrl': 'controller/login'
       ,'StartCtrl': 'controller/start'
    }
    // shortcuts for loading in controller define(['app','_s(means service)/auth'],...
   ,packages: [
        {name: '_v', location: 'vendor'}
       ,{name: '_vu', location: 'vendor/util'}
       ,{name: '_s', location: 'service'}
    ]
    // Add angular modules that does not support AMD out of the box, put it in a shim
   ,shim: {
        'app': ['angular']
       ,'angularAMD': ['angular']
       ,'angular-route': ['angular']
       ,'angular-cookies': { deps: ["angular"] }
    }
    // kick start application
   ,deps: ['bootstrap/app']
});