import mix from "./mix";
import mixins from "./mixins";

class AdderConstructor {
  constructor(x) {
    this.x = x;
  }
}

class Adder extends mix(AdderConstructor).with(...mixins) {
  constructor(...args) {
    super(...args);
  }
}

let adder = new Adder(5);
console.log(adder.add()); // 10
