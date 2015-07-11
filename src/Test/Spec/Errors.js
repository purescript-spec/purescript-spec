/* global exports */
"use strict";

// module Test.Spec.Errors

exports.errorMessage = function(err) {
  return err.message;
}

exports.errorName = function (err) {
  return err.name;
}

exports.errorStack = function (err) {
  return err.stack;
}
