/**
 * File: arraymath.js
 */
function ArrayMath(){ }

ArrayMath.prototype.add =  function(arr) {
    return arr.reduce((x,y) => x+y, 0);
};
ArrayMath.prototype.multiply =  function(arr) {
    return arr.reduce((x,y) => x*y, 1);
};

module.exports = ArrayMath;