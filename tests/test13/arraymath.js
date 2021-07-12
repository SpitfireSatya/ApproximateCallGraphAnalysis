/**
 * File: arraymath.js
 */
function ArrayMath(){ }

function unreachable2(){
  var x = 10;
}

ArrayMath.prototype.add =  function(arr) {
    return arr.reduce((x,y) => x+y, 0);
};
ArrayMath.prototype.multiply =  function(arr) {
    return arr.reduce((x,y) => x*y, 1);
};

module.exports = ArrayMath;
