/* global exports */
"use strict";

var React = require("react");
var createReactClass = require("create-react-class");

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

function readRefImpl (this_) {
  return function(name) {
    return function() {
      var refs = this_.refs || {};
      return refs[name];
    }
  }
}
exports.readRefImpl = readRefImpl;

function writeRef(this_) {
  return function(name) {
    return function(node) {
      return function() {
        var refs = this_.refs || {};
        refs[name] = node;
        this_.refs = refs;
        return {};
      }
    }
  }
}
exports.writeRef = writeRef;

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

function writeStateWithCallback(this_, cb) {
  return function(state){
    return function(cb){
      return function() {
        this_.setState({
          state: state
        }, cb);
        return state;
      };
    };
  };
}
exports.writeStateWithCallback = writeStateWithCallback;

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

  return createReactClass(result);
}
exports.createClass = createClass;

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

function createFactory(class_) {
  return React.createFactory(class_);
}
exports.createFactory = createFactory;

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
