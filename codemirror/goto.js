CodeMirror.defineMode("goto", function() {
  return {
    token: function(stream) {
      var ch = stream.next();
      stream.skipToEnd();
      return null;
    }
  };
});

CodeMirror.defineMIME("text/x-goto", "goto");
