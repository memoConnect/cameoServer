'use strict';

cameo.api = angular.module('cameoApi', ['ngResource']);

cameo.api.factory('Auth',
    function($http){
        return {
            getToken: function(auth){
                return $http({
                    method: 'GET'
                   ,url: cameo.restApi+'/token'
                   ,headers: {
                        'Authorization': 'Basic '+auth
                   }
                })
            }
        };
    });

cameo.api.factory('Talks',
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
                config = angular.extend({}
               ,{
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
        };
    });

cameo.api.factory('Assets',
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
        };
    });