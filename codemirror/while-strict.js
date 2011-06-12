CodeMirror.defineMode("while-strict", function() {
  return {
    token: function(stream) {
      var ch = stream.next();
      stream.skipToEnd();
      return null;
    }
  };
});

CodeMirror.defineMIME("text/x-while-strict", "while-strict");
