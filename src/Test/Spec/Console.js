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
