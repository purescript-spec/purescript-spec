/* global exports */
"use strict";

// module Test.Spec.Node

exports.exit = function(code) {
  return function() {
    process.exit(code);
  };
}
