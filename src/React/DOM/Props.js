/* global exports */
"use strict";

// module React.DOM.Props

exports.unsafeMkProps = function(key) {
    return function(value) {
        var result = {};
        result[key] = value;
        return result;
    };
};

exports.unsafeUnfoldProps = function(key) {
    return function(value) {
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
};
