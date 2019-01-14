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

exports.moveUpAndClearLine = function() {
  try {
    process.stderr.moveCursor(0, -1);
    process.stderr.clearLine(0);
  }
  catch (e) {}
};
