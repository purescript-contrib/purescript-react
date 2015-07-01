/* global exports */
"use strict";

// module React.DOM

function mkProps(props) {
    var result = {};
    for (var i = 0, len = props.length; i < len; i++) {
        var prop = props[i];
        var name = prop.constructor.name;
        name = name[0].toLowerCase() + name.substring(1);
        var val = prop.value0;
        /* Until React.js handles data and aria like style*/
        /* we have to unload the properties.*/
        if (name === 'data' || name === 'aria') {
            for (var subprop in val) {
                if (val.hasOwnProperty(subprop)) {
                    result[name + '-' + subprop] = val[subprop];
                }
            }
        } else {
            result[name] = val;
        }
    }
    return result;
};

exports.mkDOM = function(tagName) {
    var ctor = window.React.DOM[tagName];
    return function(props) {
        return function(children) {
            var p = props.length > 0 ? mkProps(props) : null;
            return ctor.apply(ctor, [p].concat(children));
        }
    }
};

exports.text = function(text) {
    return text;
};
