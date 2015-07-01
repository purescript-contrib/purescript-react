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

        for (var subprop in value) {
            if (value.hasOwnProperty(subprop)) {
                result[key + '-' + subprop] = value[subprop];
            }
        }

        return result;
    };
};
