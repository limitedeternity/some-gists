/*
Example async/await code:

async function genFn() {
  let num = await new Promise(resolve => setTimeout(() => resolve(123), 1000));
  return num === 123;
}

genFn.then(console.log);

This code can be transpiled to:
*/

function __awaiter(genFn) {
  const genObj = genFn();

  return (function recur(result) {
    function handler(data) {
      const nextResult = genObj.next(data);
      return nextResult.done ? Promise.resolve(nextResult.value) : recur(nextResult.value);
    }

    if (result.value instanceof Promise) {
      return result.value.then(handler);
    } else {
      throw new TypeError("It only makes sense to await Promises");
    }
  }(genObj.next()));
}

function* genFn() {
  const num = yield new Promise(resolve => setTimeout(() => resolve(123), 1000));
  return num === 123;
}

__awaiter(genFn).then(console.log);
