'use strict';
app.controller('LoginCtrl', ['$scope', '$cookieStore', '$location', 'Auth',
    function ($scope, $cookieStore, $location, Auth) {
        $scope.placeholder = {
            user: "Username"
            ,pass: "Passwort"
        };
        $scope.formData = {};
        $scope.formRes = {};

        $scope.token = $cookieStore.get("token")+" go to start"||"none";

        $scope.autologin = function(){
            $scope.formData = {
                user: "Max"
                ,pass: "moepmoep"
            };
        };

        $scope.getToken = function(){
            Auth.getToken(Base64.encode($scope.formData.user + ":" + $scope.formData.pass)).
                success(function(res){
                    $scope.formRes = res.data;
                    $cookieStore.put("token",res.data.token);
                    $scope.token = res.data.token;
                    $location.path("/start");
                }).
                error(function(res){
                    $scope.formRes = res;
                })
        };
}]);