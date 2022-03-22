export function preventDefault(event) {
  return function() {
    return event.preventDefault();
  };
}

export function isDefaultPrevented(event) {
  return function() {
    return event.isDefaultPrevented();
  };
}

export function stopPropagation(event) {
  return function() {
    return event.stopPropagation();
  };
}

export function isPropagationStopped(event) {
  return function() {
    return event.isPropagationStopped();
  };
}

export function persist(event) {
  return function() {
    return event.persist();
  };
}

export function getModifierState(key) {
  return function(event) {
    return function() {
      return event.getModifierState(key);
    };
  };
}

export function unsafeGet(key) {
  return function (event) {
    return function () {
      return event[key];
    };
  };
}
