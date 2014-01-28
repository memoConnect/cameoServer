'use strict';

cameo.ctrl = angular.module('cameoControllers', []);

cameo.ctrl.controller('AuthCtrl', ['$scope',
  function($scope) {

  }]);

cameo.ctrl.controller('AuthFormCtrl', ['$scope', '$cookieStore', '$location', 'Auth',
  function($scope, $cookieStore, $location, Auth) {
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
        //$scope.auth.$submit();
    }

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
    }
  }]);

cameo.ctrl.controller('StartCtrl', ['$scope', '$cookieStore', '$location',
    function($scope, $cookieStore, $location) {
        $scope.logout = function(){
            $cookieStore.remove("token");
            $location.path("/login");
        }
    }]);

cameo.ctrl.controller('TalksCtrl', ['$scope', '$location', 'Talks',
    function($scope, $location, Talks) {

        $scope.limit = 10;
        $scope.numberOfConversations = 0;
        $scope.conversations = [];

        Talks.getAll({offset:0, limit:$scope.limit}).
            success(function(res){
                addConversations(res.data.conversations);
                $scope.numberOfConversations = res.data.numberOfConversations;
            });

        $scope.loadMore = function(){
            Talks.getAll({offset:$scope.conversations.length, limit:$scope.limit}).
                success(function(res){
                    addConversations(res.data.conversations);
                });
        };

        function addConversations(conversations){
            angular.forEach(conversations, function(value){
                this.push(value);
            }, $scope.conversations);
        }
    }]);

cameo.ctrl.controller('ConversationCtrl', ['$scope', '$routeParams', 'Talks',
    function($scope, $routeParams, Talks) {
        $scope.conversation = {};

        Talks.getOne({conversationId:$routeParams.conversationId, offset:0, limit:10}).
            success(function(res){
                $scope.conversation = res.data;
            });
    }]);

cameo.ctrl.controller('MediaWallCtrl', ['$scope', 'Assets',
    function($scope, Assets) {
        $scope.numberOfAssets = 0;
        $scope.assets = [];

        Assets.getAll({offset:0, limit:10}).
            success(function(res){
                $scope.assets = res.data.assets;
                $scope.numberOfAssets = res.data.numberOfAssets;
            });
    }]);
