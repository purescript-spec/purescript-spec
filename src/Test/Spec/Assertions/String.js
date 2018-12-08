"use strict";

exports._startsWith = function (subs) {
  return function (str) {
    return str.startsWith(subs)
  }
}

exports._endsWith = function (subs) {
  return function (str) {
    return str.endsWith(subs)
  }
}
