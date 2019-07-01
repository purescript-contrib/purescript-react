"use strict";

var React = require("react");

exports.createRef = React.createRef;

exports.getCurrentRef_ = function(ref) {
  if (ref.current) return ref.current;
  else return ref;
}
