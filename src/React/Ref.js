"use strict";

var React = require("react");

exports.createRef = React.createRef;

exports.liftCallbackRef = function(ref) {
  return { current: ref };
}

exports.getCurrentRef_ = function(ref) {
  return ref.current;
}
