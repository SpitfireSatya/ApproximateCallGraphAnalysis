/**
 * File: main.js
 */

import { exponent as exp } from "./basicmath.js"
import * as arrayMath from "./arraymath.js";
import { assert as myassert } from "./util/Assert.js";

const res = exp(4, 5);
myassert(1024, res);

(function(v) {
  let test = function (lib, inArr){
    var math = lib;
    myassert(12, math.add(inArr));
    myassert(60, math.multiply(inArr));
  }
  test(v, [3, 4, 5]);
})(arrayMath);
