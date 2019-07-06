function evens(array) {
  return array.filter(x => !(x & 1));
}

function multiplyBy(n) {
  return (array) => array.map(x => x * n);
}

function pipe(...fns) {
  return (arg) => fns.reduce((prev, fn) => fn(prev), arg);
}

function pipeValue(arg) {
  return {
    to: (...fns) => pipe(...fns)(arg)
  }
}

pipeValue([1, 2, 3, 4, 5]).to(pipe(evens, multiplyBy(3)), console.log);
