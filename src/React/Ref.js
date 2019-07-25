"use strict";

var React = require("react");

exports.createRef = React.createRef;

exports.getCurrentRef_ = function(ref) {
  if (ref.hasOwnProperty('current')) return ref.current;
  else return ref;
}
