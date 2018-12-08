'use strict';

var React = require('react');

exports.toElementArray = function toElementArray(elements) {
  return React.createElement.apply(React, [React.Fragment, { }].concat(elements));
};

exports.childrenToArray = React.Children.toArray

exports.childrenCount = React.Children.count;
