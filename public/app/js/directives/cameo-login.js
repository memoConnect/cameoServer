'use strict';
app.directive('cameoLogin', function () {
    return  {
        restrict    :   'E',
        templateUrl :   'tpl/directives/cameo-login.html',
        scope       :   {},
        controller  :   'LoginCtrl'
    }
});