/* global exports */
"use strict";

// module React

var React = require('react');

function getProps(this_) {
  return function(){
    return this_.props;
  };
}
exports.getProps = getProps;

function getRefs(this_) {
  return function(){
    return this_.refs;
  };
}
exports.getRefs = getRefs;

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
      this_.setState({
        state: state
      });
      return state;
    };
  };
}
exports.writeState = writeState;

function readState(this_) {
  return function(){
    return this_.state.state;
  };
}
exports.readState = readState;

function transformState(this_){
  return function(update){
    return function(){
      this_.setState(function(old, props){
        return {state: update(old.state)};
      });
    };
  };
}
exports.transformState = transformState;

function createClass(spec) {
  var result = {
    displayName: spec.displayName,
    render: function(){
      return spec.render(this)();
    },
    getInitialState: function(){
      return {
        state: spec.getInitialState(this)()
      };
    },
    componentWillMount: function(){
      return spec.componentWillMount(this)();
    },
    componentDidMount: function(){
      return spec.componentDidMount(this)();
    },
    componentWillReceiveProps: function(nextProps){
      return spec.componentWillReceiveProps(this)(nextProps)();
    },
    shouldComponentUpdate: function(nextProps, nextState){
      return spec.shouldComponentUpdate(this)(nextProps)(nextState.state)();
    },
    componentWillUpdate: function(nextProps, nextState){
      return spec.componentWillUpdate(this)(nextProps)(nextState.state)();
    },
    componentDidUpdate: function(prevProps, prevState){
      return spec.componentDidUpdate(this)(prevProps)(prevState.state)();
    },
    componentWillUnmount: function(){
      return spec.componentWillUnmount(this)();
    }
  };

  return React.createClass(result);
}
exports.createClass = createClass;

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

function createFactory(class_) {
  return React.createFactory(class_);
}
exports.createFactory = createFactory;
