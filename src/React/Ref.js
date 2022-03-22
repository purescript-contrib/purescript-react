import React from "react";
export const createRef = React.createRef;

export function liftCallbackRef(ref) {
  return { current: ref };
}

export function getCurrentRef_(ref) {
  return ref.current;
}
