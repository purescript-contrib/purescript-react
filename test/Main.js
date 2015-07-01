/* global exports */
"use strict";

// module Test.Main

exports.interval = function(ms) {
    return function(action) {
        return function() {
            return setInterval(action, ms);
        }
    }
};
