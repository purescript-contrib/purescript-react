'use strict';

exports.preventDefault = function preventDefault(event) {
  return function() {
    return event.preventDefault();
  };
};

exports.isDefaultPrevented = function isDefaultPrevented(event) {
  return function() {
    return event.isDefaultPrevented();
  };
};

exports.stopPropagation = function stopPropagation(event) {
  return function() {
    return event.stopPropagation();
  };
};

exports.isPropagationStopped = function isPropagationStopped(event) {
  return function() {
    return event.isPropagationStopped();
  };
};

exports.persist = function persist(event) {
  return function() {
    return event.persist();
  };
};
