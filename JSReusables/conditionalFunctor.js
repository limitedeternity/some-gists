function isDefined(x) {
  return x !== undefined && x !== null;
}

function Conditional(condition, x) {
  return {
    then: function (fn) {
      if (condition(x)) {
        return fn(x);
      }
    }
  };
}

Conditional(isDefined, 'foo').then(console.log);
// foo
Conditional(isDefined).then(console.log);
// does nothing
