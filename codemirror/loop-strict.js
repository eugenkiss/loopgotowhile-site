CodeMirror.defineMode("loop-strict", function() {
  return {
    token: function(stream) {
      var ch = stream.next();
      stream.skipToEnd();
      return null;
    }
  };
});

CodeMirror.defineMIME("text/x-loop-strict", "loop-strict");
