/**
 * File: main.js
 */


// ---------------------------------------
class BasicMath {

  constructor(){ }

  multiply(frst, second){
	return frst * second;
  }

  exponent(base, exp){
	return Math.pow(base, exp);
  }

}

// ---------------------------------------

class ArrayMath {

  constructor(){ }

  add(arr) {
    return arr.reduce((x,y) => x+y, 0);
  }

  multiply(arr) {
    return arr.reduce((x,y) => x*y, 1);
  }

}

// ---------------------------------------


class Assert {

  //constructor(){ }

  assert(expected, actual){
    if (actual !== expected){
      console.log("expected " + expected + " but got " + actual);
    }
  }

}

// ---------------------------------------

let bm = new BasicMath();
let am = new ArrayMath();
let as = new Assert();

const res = bm.exponent(4, 5);
as.assert(1024, res);

let test = function (inArr){
  as.assert(12, am.add(inArr));
  as.assert(60, am.multiply(inArr));
}
test([3, 4, 5]);
