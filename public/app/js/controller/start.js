'use strict';
app.controller('StartCtrl', ['$scope', '$cookieStore', '$location',
function($scope, $cookieStore, $location) {
    $scope.logout = function(){
        $cookieStore.remove("token");
        $location.path("/login");
    };
}]);