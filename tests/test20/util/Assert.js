/**
 * File: util/assert.js
 */

export class Assert {

  // unreachable
  constructor(){
    console.log("invoked!");
  }

  static assert(expected, actual){
    if (actual !== expected){
      console.log("expected " + expected + " but got " + actual);
    }
  }
}

export class UnreachableClass {

  constructor(){ }

  foo(i){ i = 1; return i }

  static bar(){ }

}
