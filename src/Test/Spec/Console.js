/* global exports */
"use strict";

// module Test.Spec.Console

function hasProcessWrite() {
  return process &&
      process.stdout &&
      typeof process.stdout.write === 'function'
}

exports.write = function(s) {
  return function () {
    if (hasProcessWrite()) {
      process.stdout.write(s);
    }
  };
};

// This needs a foreign function to support the escape sequence.
exports._setAttr = function (codeStr) {
  return exports.write("\x1b[" + codeStr + "m");
};
