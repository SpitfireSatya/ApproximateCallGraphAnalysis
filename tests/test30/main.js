/**
 * File: main.js
 */

import {BasicMath as B} from "./basicmath.js"
import {ArrayMath as A} from "./arraymath.js";
import {Assert as ASSERT} from "./util/Assert.js";

let bm = new B();
let am = new A();
let as = new ASSERT();

const res = bm.exponent(4, 5);
as.assert(1024, res);

let test = function (inArr){
  as.assert(12, am.add(inArr));
  as.assert(60, am.multiply(inArr));
}
test([3, 4, 5]);
