/**
 * File: arraymath.js
 */

export default class ArrayMath {

  static add(arr) {
    return arr.reduce((x,y) => x+y, 0);
  }

  static multiply(arr) {
    return arr.reduce((x,y) => x*y, 1);
  }

}
