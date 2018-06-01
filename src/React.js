/* global exports */
"use strict";

var React = require("react");

function createClass(baseClass) {
  function bindProperty(instance, prop, value) {
    switch (prop) {
      case 'componentDidMount':
      case 'componentDidUpdate':
      case 'componentWillMount':
      case 'componentWillUnmount':
      case 'render':
      case 'state':
        instance[prop] = value;
        break;

      case 'componentWillReceiveProps':
        instance[prop] = function (a) { return value(a)(); };
        break;

      case 'componentDidCatch':
      case 'componentWillUpdate':
      case 'shouldComponentUpdate':
        instance[prop] = function (a, b) { return value(a)(b)(); };
        break;

      default:
        throw new Error('Not a component property: ' + prop);
    }
  }

  return function (displayName) {
    return function (ctrFn) {
      var Constructor = function (props) {
        baseClass.call(this, props);
        var spec = ctrFn(this)();
        for (var k in spec) {
          bindProperty(this, k, spec[k]);
        }
      };

      Constructor.displayName = displayName;
      Constructor.prototype = Object.create(baseClass.prototype);
      Constructor.prototype.constructor = Constructor;

      return Constructor;
    };
  };
}

exports.componentImpl = createClass(React.Component);

exports.pureComponentImpl = createClass(React.PureComponent);

exports.statelessComponent = function(x) { return x; };

exports.fragment = React.Fragment;

function getProps(this_) {
  return function(){
    return this_.props;
  };
}
exports.getProps = getProps;

exports.childrenToArray = React.Children.toArray

exports.childrenCount = React.Children.count;

function setStateImpl(this_) {
  return function(state){
    return function(){
      this_.setState(state);
    };
  };
}
exports.setStateImpl = setState;

function setStateWithCallbackImpl(this_, cb) {
  return function(state){
    return function(cb){
      return function() {
        this_.setState(state, cb);
      };
    };
  };
}
exports.setStateWithCallbackImpl = setStateWithCallback;

function getState(this_) {
  return function(){
    return this_.state;
  };
}
exports.getState = getState;

function transformState(this_){
  return function(update){
    return function(){
      this_.setState(function(old, props){
        return update(old);
      });
    };
  };
}
exports.transformState = transformState;

function forceUpdateCbImpl(this_, cb) {
  this_.forceUpdate(function() {
    return cb();
  });
  return {};
};
exports.forceUpdateCbImpl = forceUpdateCbImpl;

function createElement(class_) {
  return function(props){
    return function(children){
      return React.createElement.apply(React, [class_, props].concat(children));
    };
  };
}
exports.createElementImpl = createElement;
exports.createElementTagName = createElement;

function createLeafElement(class_) {
  return function(props) {
    return React.createElement(class_, props);
  };
}
exports.createLeafElementImpl = createLeafElement;

function createElementDynamic(class_) {
  return function(props) {
    return function(children){
      return React.createElement(class_, props, children);
    };
  };
};
exports.createElementDynamicImpl = createElementDynamic;
exports.createElementTagNameDynamic = createElementDynamic;
