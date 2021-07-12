/**
 * File: arraymath.js
 */

export class ArrayMath {

  constructor(){ }

  add(arr) {
    return arr.reduce((x,y) => x+y, 0);
  }

  multiply(arr) {
    return arr.reduce((x,y) => x*y, 1);
  }

}
