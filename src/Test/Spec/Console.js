/* global exports */
"use strict";

// module Test.Spec.Console

exports.write = function(s) {
  return function () {
    if (process) {
      process.stdout.write(s);
    }
  };
}
