/* global exports */
"use strict";

// module Test.Spec.Console

exports.write = function(s) {
  return function () {
    try {
      process.stdout.write(s);
    }
    catch (e) {}
  };
};

exports.moveUpAndClearDown = function(lines) {
  return function() {
    try {
      process.stderr.moveCursor(0, -lines);
      process.stderr.clearScreenDown();
    }
    catch (e) {}
  };
};
