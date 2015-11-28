/* global exports */
"use strict";

// module React.DOM.Props

var React = require('react');

function unsafeMkProps(key) {
  return function(value){
    var result = {};
    result[key] = value;
    return result;
  };
}
exports.unsafeMkProps = unsafeMkProps;

function unsafeUnfoldProps(key) {
  return function(value){
    var result = {};
    var props = {};
    props[key] = result;

    for (var subprop in value) {
      if (value.hasOwnProperty(subprop)) {
          result[subprop] = value[subprop];
      }
    }

    return props;
  };
}
exports.unsafeUnfoldProps = unsafeUnfoldProps;
