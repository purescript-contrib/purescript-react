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

function unsafePrefixProps(prefix) {
  return function(value){
    var result = {};

    for (var prop in value) {
      if (value.hasOwnProperty(prop)) {
        result[prefix + prop] = value[prop];
      }
    }

    return result;
  };
}
exports.unsafePrefixProps = unsafePrefixProps;

function unsafeFromPropsArray(props) {
  var result = {};

  for (var i = 0, len = props.length; i < len; i++) {
    var prop = props[i];

    for (var key in prop) {
      if (prop.hasOwnProperty(key)) {
        result[key] = prop[key];
      }
    }
  }

  return result;
};
exports.unsafeFromPropsArray = unsafeFromPropsArray;
