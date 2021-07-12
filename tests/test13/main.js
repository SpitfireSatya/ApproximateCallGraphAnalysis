/**
 * File: main.js
 */
const basicMath = require('./basicmath.js');
const arrayMath = require('./arraymath.js');
const __dir = './util/'
const assert = require(__dir + 'Assert.js');

const res = basicMath.exponent(4, 5);
assert(1024, res);

function unreachable4(){
  var x = function(){};
}

(function(v) {
  let test = function (lib, inArr){
    var math = new lib();
    assert(12, math.add(inArr));
    assert(60, math.multiply(inArr));
  }
  test(v, [3, 4, 5]);
})(arrayMath);
