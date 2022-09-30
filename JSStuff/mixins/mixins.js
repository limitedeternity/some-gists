class Mixin1 {
  constructor() {
    this.y = 5;
  }
}

class Mixin2 {
  add() {
    return this.x + this.y;
  }
}

const mixins = [Mixin1, Mixin2];
export default mixins;
