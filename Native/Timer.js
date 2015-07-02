Elm.Native.Timer = {};
Elm.Native.Timer.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};
  var Timer = localRuntime.Native.Timer = localRuntime.Native.Timer || {};

  var Task = Elm.Native.Task.make(localRuntime);
  var Utils = Elm.Native.Utils.make(localRuntime);

  if (!Timer.values) {
    Timer.values = (function () {
      var handlers = {};
      return {
        start: F3(function (id, duration, interval) {
          return Task.asyncFunction(function (callback) {
            var start = new Date().getTime();
            if (handlers[id] != null) {
              callback(Task.fail({message: "Timer '" + id + "' already running."}));
            } else {
              handlers[id] = setInterval(
                function () {
                  var rem = duration - (new Date().getTime() - start);
                  app.ports[id].send(Math.max(0, rem));
                },
                interval
              );
              callback(Task.succeed(Utils.Tuple0));
            }
          });
        }),
        stop: function (id) {
          return Task.asyncFunction(function (callback) {
            if (handlers[id] == null) {
              callback(Task.fail({message: "Timer '" + id + "' not running."}));
            } else {
              clearInterval(handlers[id]);
              delete handlers[id];
              callback(Task.succeed(Utils.Tuple0));
            }
          });
        },
        heat: Task.asyncFunction(function (callback) {
          callback(Task.succeed(Utils.Tuple0));
        }),
        alert: function (msg) {
          return Task.asyncFunction(function (callback) {
            alert(msg);
            callback(Task.succeed(Utils.Tuple0));
          });
        }
      };
    })();
  }

  return Timer.values;
};
