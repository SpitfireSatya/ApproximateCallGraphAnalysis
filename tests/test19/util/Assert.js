/**
 * File: util/assert.js
 */

export default class Assert {

  static assert(expected, actual){
    if (actual !== expected){
      console.log("expected " + expected + " but got " + actual);
    }
  }

}
