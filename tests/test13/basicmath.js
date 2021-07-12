/**
 * file: basicmath.js
 */

function unreachable1(){
  var x = 10;
}

module.exports.multiply = function(frst, second){
	return frst * second;
};

module.exports.exponent = function (base, exp){
	return Math.pow(base, exp);
};
