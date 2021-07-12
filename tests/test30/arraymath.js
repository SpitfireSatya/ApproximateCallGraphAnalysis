/**
 * File: arraymath.js
 */

class A {

  constructor(){ }

  add(arr) {
    return arr.reduce((x,y) => x+y, 0);
  }

  multiply(arr) {
    return arr.reduce((x,y) => x*y, 1);
  }

}

export { A as ArrayMath }
