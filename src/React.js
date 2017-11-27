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

function getProps(this_) {
  return function(){
    return this_.props;
  };
}
exports.getProps = getProps;

function childrenToArray(children) {
  var result = [];

  React.Children.forEach(children, function(child){
    result.push(child);
  });

  return result;
}
exports.childrenToArray = childrenToArray;

function getChildren(this_) {
  return function(){
    var children = this_.props.children;

    var result = childrenToArray(children);

    return result;
  };
}
exports.getChildren = getChildren;

function writeState(this_) {
  return function(state){
    return function(){
      this_.setState(state);
      return state;
    };
  };
}
exports.writeState = writeState;

function writeStateWithCallback(this_, cb) {
  return function(state){
    return function(cb){
      return function() {
        this_.setState(state, cb);
        return state;
      };
    };
  };
}
exports.writeStateWithCallback = writeStateWithCallback;

function readState(this_) {
  return function(){
    return this_.state;
  };
}
exports.readState = readState;

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

function handle(f) {
  return function(e){
    return f(e)();
  };
};
exports.handle = handle;

function createElement(class_) {
  return function(props){
    return function(children){
      return React.createElement.apply(React, [class_, props].concat(children));
    };
  };
}
exports.createElement = createElement;
exports.createElementTagName = createElement;

function createElementDynamic(class_) {
  return function(props) {
    return function(children){
      return React.createElement(class_, props, children);
    };
  };
};
exports.createElementDynamic = createElementDynamic;
exports.createElementTagNameDynamic = createElementDynamic;

function preventDefault(event) {
  return function() {
    event.preventDefault();
  };
};
exports.preventDefault = preventDefault;

function stopPropagation(event) {
  return function() {
    event.stopPropagation();
  };
};
exports.stopPropagation = stopPropagation;
