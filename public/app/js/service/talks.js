'use strict';
app.factory('Talks',
function($http){
    return {
        getAll: function(config){
            angular.extend(config, {}, {
                offset:0
               ,limit:10
            });
            return $http({
                method: 'GET'
               ,url: cameo.restApi+'/conversations?token='+cameo.token+"&offset="+config.offset+"&limit="+config.limit
            })
        }
        ,getOne: function(config){
            config = angular.extend({}, {
                    conversationId: null
                   ,offset:0
                   ,limit:10
                }
               ,config);

            return $http({
                method: 'GET'
               ,url: cameo.restApi+'/conversation/'+config.conversationId+'?token='+cameo.token+"&offset="+config.offset+"&limit="+config.limit
            })
        }
    }
});