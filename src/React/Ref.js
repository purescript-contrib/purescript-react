'use strict';

var React = require('react');

exports.createRef = function createRef() {
  return React.createRef();
};

exports.forwardRef_ = function forwardRef(render) {
  return React.forwardRef(render);
}

exports.getRef_ = function getRef_(ref) {
  return ref.current;
}

exports.setRef_ = function setRef_(ref, value) {
  ref.current = value;
}
