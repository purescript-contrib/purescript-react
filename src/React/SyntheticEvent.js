'use strict';

exports.unsafeGet = function unsafeGet(key) {
  return function (event) {
    return function () {
      return event[key];
    };
  };
};

exports.unsafeGetFn = function unsafeGetFn(key) {
  return function (event) {
    return function () {
      return event[key].bind(event);
    };
  };
};
