/**
 * File: main.js
 */
const basicMath = require('../../basicmath');
const arrayMath = require('../../arraymath.js');
 
const assert = require('../../util/Assert');

const res = basicMath.exponent(4, 5);
assert(1024, res);

(function(v) {
  let test = function (lib, inArr){
    var math = new lib();
    assert(12, math.add(inArr));
    assert(60, math.multiply(inArr));
  }
  test(v, [3, 4, 5]);
})(arrayMath);