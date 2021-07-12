/**
 * File: main.js
 */

import { exponent } from "./basicmath.js"
import { arrayMath } from "./arraymath.js";
import { assert } from "./assert.js";

function unreachable1(){
  var x = 10;
}

const res = exponent(4, 5);
assert(1024, res);

(function(v) {
  let test = function (lib, inArr){
    var math = lib;
    assert(12, math.add(inArr));
    assert(60, math.multiply(inArr));
  }
  test(v, [3, 4, 5]);
})(arrayMath);
