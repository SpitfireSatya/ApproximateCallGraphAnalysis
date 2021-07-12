const m = {
  x: 42,
  getX: function() {
    return this.x;
  }
};

const unboundGetX = m.getX;
console.log(unboundGetX()); // The function gets invoked at the global scope
// expected output: undefined

const boundGetX = unboundGetX.bind(m);
console.log(boundGetX());
// expected output: 42
