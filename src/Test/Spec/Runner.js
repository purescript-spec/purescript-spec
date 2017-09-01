/* global exports */
"use strict";

// module Test.Spec.Runner

exports.dateNow = function () {
  return Date.now();
}

exports.exit = function(code) {
  return function() {
    try {
      if (process && typeof process.exit === 'function') {
        process.exit(code);
      }
    } catch(e) {}
  };
}
