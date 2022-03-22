function unsafeMkProps(key) {
  return function(value){
    var result = {};
    result[key] = value;
    return result;
  };
}
export {unsafeMkProps};

function unsafeUnfoldProps(key) {
  return function(value){
    var result = {};
    var props = {};
    props[key] = result;

    for (var subprop in value) {
      if (Object.hasOwnProperty.apply(value, [subprop])) {
        result[subprop] = value[subprop];
      }
    }

    return props;
  };
}
export {unsafeUnfoldProps};

function unsafePrefixProps(prefix) {
  return function(value){
    var result = {};

    for (var prop in value) {
      if (Object.hasOwnProperty.apply(value, [prop])) {
        result[prefix + prop] = value[prop];
      }
    }

    return result;
  };
}
export {unsafePrefixProps};

function unsafeFromPropsArray(props) {
  var result = {};

  for (var i = 0, len = props.length; i < len; i++) {
    var prop = props[i];

    for (var key in prop) {
      if (Object.hasOwnProperty.apply(prop, [key])) {
        result[key] = prop[key];
      }
    }
  }

  return result;
}
export {unsafeFromPropsArray};
