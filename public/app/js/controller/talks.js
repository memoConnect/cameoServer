'use strict';
app.controller('TalksCtrl', ['$scope', '$location', 'Talks',
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