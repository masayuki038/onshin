var scroll_down_chatlogs = function() {
  var main = $('#main')[0];
  if(main) {
    $('#main').innerHeight(window.innerHeight - 60 - 210);
    main.scrollTop = main.scrollHeight;
  }
}

var app = angular.module('app', ['ngSanitize']);

app.run(function() {
  $(window).bind('resize', function(e){
    scroll_down_chatlogs();
  });
});

app.directive('ngEnter', function() {
  return function (scope, element, attrs) {
    element.bind("keypress", function (event) {
      if (event.which == 13 && event.shiftKey) {
        scope.$apply(function(){
          scope.$eval(attrs.ngEnter);
        });
        return false;
      }
    });
  };
});

app.factory('ChatService', function() {
  var service = {};
 
  service.connect = function(uri, callback) {
    if(service.server) {
      if(callback) {
        callback();
      } 
      return; 
    }
 
    var server = new FancyWebSocket(uri);
    server.bind('open', function() {
      if(callback) {
        callback();
      }
    });

    server.bind('close', function() {
      alert("Connection closed. Please reload this page.");
    });

    server.bind('message', function(data) {
      service.message_callback(data);  
    });

    server.bind('authenticated', function(data) {
      service.auth_callback(data);
    });

    server.bind('unauthenticated', function(data) {
      service.unauth_callback(data);
    });

    server.bind('update_status', function(data) {
      service.update_status_callback(data);
    });

    service.server = server;
  }

  service.send_message = function(message) {
    var obj = {message: message, user: sessionStorage.getItem("name")};
    service.server.send('send_message', obj);
  }

  service.login = function(mail, password, name, update) {
    var obj = {mail: mail, password: password, name: name, update: update};
    service.server.send('authenticate', obj);
  }

  service.try_reconnect = function(uri, callback) {
    return service.connect(uri, function() {
      var mail = sessionStorage.getItem("mail");
      if(!mail) {
        callback();
        return;
      }
      var obj = {mail: mail, token: sessionStorage.getItem("token")};
      service.server.send('reconnect', obj);
    });
  }

  service.on_message = function(callback) {
    service.message_callback = callback;
  }

  service.on_authenticated = function(callback) {
    service.auth_callback = callback;
  }

  service.on_unauthenticated = function(callback) {
    service.unauth_callback = callback;
  }

  service.on_update_status = function(callback) {
    service.update_status_callback = callback;
  }

  return service;
});

app.filter('sanitize', function($sanitize) {
  return function(input) {
    return $sanitize(input);
  }
});    

app.filter('convert_linefeed', function() {
  return function(input) {
    return input.replace(/&#10;/g, '<br/>');
  }
});

function ChatCtrl($scope, $sanitize, ChatService) {
  $scope.messages = [];
  $scope.active = true;
  $scope.unread = 0;
  $scope.states = [];

  window.onblur = function() {
    $scope.inactivate();
  }

  window.onfocus = function() {
    $scope.activate();
  }
 
  $scope.start = function(uri) {
    $scope.uri = uri;
    ChatService.try_reconnect(uri, $scope.show_login_dialog);
  }

  ChatService.on_message(function(data) {
    for(var i = 0; i < data.length; i++) {
      $scope.messages.push(data[i]);
    }
    if(!$scope.active) {
      $scope.unread += data.length;
      document.title = "onshin(" + $scope.unread + ")";
    }
    $scope.$apply();
    scroll_down_chatlogs();
  });

  ChatService.on_update_status(function(data) {
    $scope.states = data.states;
    $scope.$apply();
  });

  $scope.show_login_dialog = function() {
    $('#login_dialog').on('shown', function () {
      $('#profile_mail').focus();
    });
    $('#login_dialog').modal('show');
  }
 
  $scope.send_message = function(message) {
    var content = chomp(message);
    ChatService.send_message(content);   
  }

  $scope.inactivate = function() {
    $scope.active = false;
  }

  $scope.activate = function() {
    $scope.active = true;
    $scope.unread = 0;
    window.setTimeout(function () { $(document).attr("title", "onshin"); }, 200);
  }
 
  $scope.set_mention = function(member) {
    var message = $scope.message;
    if(!message) {
        message = "";
    }
    $scope.message = message + "@" + member + " ";
  }

  var chomp = function(str) {
    if(str == undefined || str == null) {
      return str;
    }
    return str.replace(/(\n|\r)+$/, '');
  }
}

function LoginCtrl($scope, ChatService) {
  $scope.has_error = false;
  $scope.error = undefined;
  $scope.update_account = false;

  $scope.mail = "";
  $scope.password = "";
  $scope.name = "";

  ChatService.on_authenticated(function(data) {
    var storage = sessionStorage;
    storage.setItem('token', data.token);
    storage.setItem('name', data.name);
    storage.setItem('mail', data.mail);
    $('#login_dialog').modal('hide');
  });

  ChatService.on_unauthenticated(function(data) {
    $scope.show_login_dialog();
    $scope.has_error = true;
    $scope.error = data.error;
    $scope.$apply();
  });

  $scope.save_change = function() {
    ChatService.connect($scope.uri, function() {
      ChatService.login($scope.mail, $scope.password, $scope.name, $scope.update_account);
    });
  }
}
