/* global exports */
"use strict";

// module Test.Spec.Color

// This needs a foreign function to support the escape sequence.
// note: we may also want to check if colors are supported here (TODO)
exports._colored = function (c) {
  return function(s) {
    return "\x1b[" + c + "m" + s + "\x1b[0m";
  }
}
