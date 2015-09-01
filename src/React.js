/* global exports */
"use strict";

// module React

exports.getProps = function(ctx) {
    return function() {
        return ctx.props;
    };
};

exports.getRefs = function(ctx) {
    return function() {
        return ctx.refs;
    };
};

exports.getChildren = function(ctx) {
  return function() {
    return ctx.props.children;
  };
};

exports.writeState = function(ctx) {
    return function(state) {
        return function() {
            ctx.replaceState({
                state: state
            });
            return function() {
                return state;
            }
        };
    };
};

exports.readState = function(ctx) {
    return function() {
        return ctx.state.state;
    };
};

exports.mkUI = function(ss) {
    var result = {};
    for (var s in ss) {
        if (ss.hasOwnProperty(s)) {
          if (s === "displayName") {
            result[s] = ss[s];
          }
          else if (s === "componentWillReceiveProps") {
            result[s] = (function(impl) {
                return function(nextProps) {
                    return impl(this)(nextProps)();
                }
            })(ss[s]);
          }
          else if (s === "shouldComponentUpdate") {
            result[s] = (function(impl) {
                return function(nextProps, nextState) {
                    return impl(this)(nextProps)(nextState.state)();
                }
            })(ss[s]);
          }
          else if (s === "componentWillUpdate") {
            result[s] = (function(impl) {
                return function(nextProps, nextState) {
                    return impl(this)(nextProps)(nextState.state)();
                }
            })(ss[s]);
          }
          else if (s === "componentDidUpdate") {
            result[s] = (function(impl) {
                return function(prevProps, prevState) {
                    return impl(this)(prevProps)(prevState.state)();
                }
            })(ss[s]);
          }
          else {
            result[s] = (function(impl) {
                return function() {
                    return impl(this)();
                }
            })(ss[s]);
          }
        }
    }
    result.getInitialState = function() {
        return {
            state: ss.getInitialState(this)()
        };
    };
    return React.createClass(result);
};

exports.handle = function(f) {
    return function(e) {
        return f(e)();
    };
};

exports.renderToString = React.renderToString;

exports.renderToBody = function(component) {
    return function() {
        return React.render(component, document.body);
    }
};

exports.renderToElementById = function(id) {
    return function(component) {
        return function() {
            return React.render(component, document.getElementById(id));
        }
    }
};

exports.createElement = function(factory) {
  return function(props) {
    return function(children){
      return React.createElement.apply(React, [factory, props].concat(children));
    };
  };
};
