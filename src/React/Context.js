'use strict';

var React = require('react');

exports.getProvider = function getProvider(context) {
  return context.Provider;
};

exports.getConsumer = function getConsumer(context) {
  return context.Consumer;
};

exports.createContext_ = function createContext_(defaultValue, calculateChangedBits) {
  return calculateChangedBits ?
    React.createContext(defaultValue, calculateChangedBits) :
    React.createContext(defaultValue)
  ;
};
