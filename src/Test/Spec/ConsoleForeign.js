/* global exports */
"use strict";

// module Test.Spec.ConsoleForeign

function hasProcessWrite() {
  return process &&
      process.stdout &&
      typeof process.stdout.write === 'function'
}

exports.supportedEnvironment = hasProcessWrite();

exports.write = function(s) {
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
  return exports.write("\x1b[" + codeStr + "m");
};
