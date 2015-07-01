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
    return function(render) {
        var specs = {};
        for (var s in ss) {
            if (ss.hasOwnProperty(s)) {
                specs[s] = (function(impl) {
                    return function() {
                        return impl(this)();
                    }
                })(ss[s]);
            }
        }
        specs.getInitialState = function() {
            return {
                state: ss.getInitialState(this)()
            };
        };
        specs.render = function() {
            return render(this)();
        };
        return React.createClass(specs);
    }
};

exports.handle = function(f) {
    return function(e) {
        return f(e)();
    };
};

exports.renderToString = React.renderComponentToString;

exports.renderToBody = function(component) {
    return function() {
        return React.renderComponent(component, document.body);
    }
};

exports.renderToElementById = function(id) {
    return function(component) {
        return function() {
            return React.renderComponent(component, document.getElementById(id));
        }
    }
};
