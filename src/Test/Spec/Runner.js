/* global exports */
"use strict";

// module Test.Spec.Runner

exports.exit = function(code) {
  return function() {
    if (process && typeof process.exit === 'function') {
      process.exit(code);
    }
  };
}
