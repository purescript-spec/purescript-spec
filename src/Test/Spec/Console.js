/* global exports */
"use strict";

// module Test.Spec.Console

function hasProcessWrite() {
  try {
    return process &&
        process.stdout &&
        typeof process.stdout.write === 'function'
  }
  catch(e) {
    return false
  }
}

exports.write = function(s) {
  return function () {
    if (hasProcessWrite()) {
      try {
        process.stdout.write(s);
      }
      catch (e) {}
    }
  };
};
