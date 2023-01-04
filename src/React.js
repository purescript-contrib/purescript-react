import React from "react";

function createClass(baseClass) {
  function invoke1(f) {
    return f === undefined ? f : function (a) {
      return f(a)()
    }
  }
  function invoke2(f) {
    return f === undefined ? f : function (a, b) {
      return f(a)(b)()
    }
  }
  function invoke3(f) {
    return f === undefined ? f : function (a, b, c) {
      return f(a)(b)(c)()
    }
  }

  return function (displayName) {
    return function (ctrFn) {
      var Constructor = function (props) {
        baseClass.call(this, props);
        var spec = ctrFn(this)();

        this.state = spec.state;
        this.render = spec.render;
        this.componentDidMount = spec.componentDidMount;
        this.componentWillUnmount = spec.componentWillUnmount;
        this.componentDidCatch = invoke2(spec.componentDidCatch);
        this.componentWillUpdate = invoke2(spec.componentWillUpdate);
        this.shouldComponentUpdate = invoke2(spec.shouldComponentUpdate);
        this.getSnapshotBeforeUpdate = invoke2(spec.getSnapshotBeforeUpdate);
        this.componentDidUpdate = invoke3(spec.componentDidUpdate);
        this.UNSAFE_componentWillMount = spec.unsafeComponentWillMount;
        this.UNSAFE_componentWillReceiveProps = invoke1(spec.unsafeComponentWillReceiveProps);
        this.UNSAFE_componentWillUpdate = invoke2(spec.unsafeComponentWillUpdate);
      };

      Constructor.displayName = displayName;
      Constructor.prototype = Object.create(baseClass.prototype);
      Constructor.prototype.constructor = Constructor;

      return Constructor;
    };
  };
}

var componentImpl = createClass(React.Component);
export {componentImpl};

// eslint-disable-next-line no-unused-vars
function createClassWithDerivedState(classCtr) {
  return function(displayName) {
    return function(getDerivedStateFromProps) {
      return function(ctrFn) {
        var Constructor = componentImpl(displayName)(ctrFn);
        Constructor.getDerivedStateFromProps = function(a, b) { return getDerivedStateFromProps(a)(b); };
        return Constructor;
      };
    };
  };
}

export const componentWithDerivedStateImpl = createClassWithDerivedState(componentImpl);

var pureComponentImpl = createClass(React.PureComponent);
export {pureComponentImpl};
export const pureComponentWithDerivedStateImpl = createClassWithDerivedState(pureComponentImpl);
export function statelessComponent(x) { return x; }
export const fragment = React.Fragment;

function getProps(this_) {
  return function(){
    return this_.props;
  };
}
export {getProps};
export const childrenToArray = React.Children.toArray;
export const childrenCount = React.Children.count;

function setStateImpl(this_) {
  return function(state){
    return function(){
      this_.setState(state);
    };
  };
}
export {setStateImpl};

function setStateWithCallbackImpl(this_) {
  return function(state){
    return function(cb){
      return function() {
        this_.setState(state, cb);
      };
    };
  };
}
export {setStateWithCallbackImpl};

function getState(this_) {
  return function(){
    if (!this_.state) {
      throw new Error("[purescript-react] Cannot get state within constructor");
    }
    return this_.state;
  };
}
export {getState};

function forceUpdateWithCallback(this_) {
  return function(cb) {
    return function() {
      this_.forceUpdate(cb);
    };
  };
}
export {forceUpdateWithCallback};

function createElement(class_) {
  return function(props){
    return function(children){
      return React.createElement.apply(React, [class_, props].concat(children));
    };
  };
}
export {createElement as createElementImpl};
export {createElement as createElementTagName};

function createLeafElement(class_) {
  return function(props) {
    return React.createElement(class_, props);
  };
}
export {createLeafElement as createLeafElementImpl};

function createElementDynamic(class_) {
  return function(props) {
    return function(children){
      return React.createElement(class_, props, children);
    };
  };
}
export {createElementDynamic as createElementDynamicImpl};
export {createElementDynamic as createElementTagNameDynamic};

function createContext(defaultValue) {
  var context = React.createContext(defaultValue);
  return {
    consumer: context.Consumer,
    provider: context.Provider
  };
}
export {createContext};

export var emptyReactElement = null;

function isEmptyReactElement(a) {
  return a === emptyReactElement;
};

export {isEmptyReactElement};
