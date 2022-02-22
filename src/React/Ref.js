"use strict";

import React from "react";
export var createRef = React.createRef;

export function liftCallbackRef(ref) {
  return { current: ref };
}

export function getCurrentRef_(ref) {
  return ref.current;
}
