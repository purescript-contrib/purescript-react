'use strict';

var React = require('react');

exports.useState_ = function useState_(Tuple, initialState) {
  var result = React.useState(initialState);

  var state = result[0];

  var setState = result[1];

  var tuple = Tuple(state)(setState);

  return tuple;
};

exports.useEffect_ = function useEffect_(effect, inputs) {
  var result = inputs ? React.useEffect(effect, inputs) : React.useEffect(effect);

  return result;
};

exports.useContext_ = function useContext_(context) {
  var result = React.useContext(context);

  return result;
}

exports.useReducer_ = function useReducer_(Tuple, reducer, initialState) {
  var result = React.useReducer(reducer, initialState);

  var state = result[0];

  var dispatch = result[1];

  var tuple = Tuple(state)(dispatch);

  return tuple;
};

exports.useReducerLazy_ = function useReducerLazy_(Tuple, reducer, initialState, initialAction) {
  var result = React.useReducer(reducer, initialState, initialAction);

  var state = result[0];

  var dispatch = result[1];

  var tuple = Tuple(state)(dispatch);

  return tuple;
};

exports.useCallback_ = function useCallback_(callback, inputs) {
  var result = inputs ? React.useCallback(callback, inputs) : React.useCallback(callback);

  return result;
};

exports.useMemo_ = function useMemo_(memo, inputs) {
  var result = inputs ? React.useMemo(memo, inputs) : React.useMemo(memo);

  return result;
};

exports.useRef_ = function useRef_(initialValue) {
  var result = React.useRef(initialValue);

  return result;
}

exports.getRef_ = function getRef_(ref) {
  return ref.current;
}

exports.setRef_ = function setRef_(ref, value) {
  ref.current = value;
}

exports.useImperativeMethods_ = function useImperativeMethods_(ref, imperativeMethods, inputs) {
  var result = inputs ? React.useImperativeMethods(ref, imperativeMethods, inputs) : React.useImperativeMethods(ref, imperativeMethods);

  return result;
};

exports.useMutationEffect_ = function useMutationEffect_(mutationEffect, inputs) {
  var result = inputs ? React.useMutationEffect(mutationEffect, inputs) : React.useMutationEffect(mutationEffect);

  return result;
};

exports.useLayoutEffect_ = function useLayoutEffect_(layoutEffect, inputs) {
  var result = inputs ? React.useLayoutEffect(layoutEffect, inputs) : React.useLayoutEffect(layoutEffect);

  return result;
};
