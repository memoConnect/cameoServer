'use strict';
app.factory('Assets',
function($http){
    return {
        getAll: function(config){
            angular.extend(config, {}, {
                offset:0
               ,limit:10
            });

            return $http({
                method: 'GET'
               ,url: cameo.restApi+'/media?token='+cameo.token+"&offset="+config.offset+"&limit="+config.limit
            })
        }
    }
});