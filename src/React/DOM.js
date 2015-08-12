/* global exports */
"use strict";

// module React.DOM

function mkProps(props) {
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

exports.mkDOM = function(tagName) {
    return function(props) {
        return function(children) {
            return React.createElement(tagName, props.length > 0 ? mkProps(props) : null, children);
        }
    }
};

exports.text = function(text) {
    return text;
};
