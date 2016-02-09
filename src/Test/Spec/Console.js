/* global exports */
"use strict";

// module Test.Spec.Console

function hasProcessWrite() {
  return process &&
      process.stdout &&
      typeof process.stdout.write === 'function'
}

function write(s) {
  return function () {
    if (hasProcessWrite()) {
      process.stdout.write(s);
    }
  };
};

exports.consoleLog = function(s) {
  return function() {
    console.log(s);
  }
}

// This needs a foreign function to support the escape sequence.
exports._setAttr = function (codeStr) {
  return write("\x1b[" + codeStr + "m");
};
