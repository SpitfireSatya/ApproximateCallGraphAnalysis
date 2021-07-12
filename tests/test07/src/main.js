/**
 * File: main.js
 */

import { exponent } from "./basicmath.js"
import arrayMath from "./arraymath.js";
import { my_assert } from "./assert.js";

const res = exponent(4, 5);
my_assert(1024, res);

(function(v) {
  let test = function (lib, inArr){
    var math = arrayMath;
    my_assert(12, math.add(inArr));
    my_assert(60, math.multiply(inArr));
  }
  test(v, [3, 4, 5]);
})(arrayMath);
