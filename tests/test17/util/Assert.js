/**
 * File: util/assert.js
 */

export default class Assert {

  //constructor(){ }

  assert(expected, actual){
    if (actual !== expected){
      console.log("expected " + expected + " but got " + actual);
    }
  }

}
