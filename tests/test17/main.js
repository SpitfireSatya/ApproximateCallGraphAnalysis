/**
 * File: main.js
 */

import BasicMath from "./basicmath.js"
import ArrayMath from "./arraymath.js";
import Assert from "./util/Assert.js";

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
