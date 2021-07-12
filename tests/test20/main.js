/**
 * File: main.js
 */

import BasicMath from "./basicmath.js"
import ArrayMath from "./arraymath.js";
import {Assert} from "./util/Assert.js";

let bm = new BasicMath();
let am = new ArrayMath();

const res = bm.exponent(4, 5);
Assert.assert(1024, res);

let test = function (inArr){
  Assert.assert(12, am.add(inArr));
  Assert.assert(60, am.multiply(inArr));
}
test([3, 4, 5]);
