/* global exports */
"use strict";

// module Test.Spec.Console

exports.write = function(s) {
  return function () {
    if (process &&
        process.stdout &&
        typeof process.stdout.write === 'function') {
      process.stdout.write(s);
    }
  };
}
