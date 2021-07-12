/**
 * File: main.js
 */

import BasicMath from "./basicmath.js"
import ArrayMath from "./arraymath.js";
import Assert from "./util/Assert.js";

const res = BasicMath.exponent(4, 5);
Assert.assert(1024, res);

let test = function (inArr){
  Assert.assert(12, ArrayMath.add(inArr));
  Assert.assert(60, ArrayMath.multiply(inArr));
}
test([3, 4, 5]);
