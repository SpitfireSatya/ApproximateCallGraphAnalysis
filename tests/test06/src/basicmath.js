/**
 * file: basicmath.js
 */
let multiply = function (frst, second){
	return frst * second;
}

var exponent = function (base, exp){ // export default let ...
	return Math.pow(base, exp);
};

export { exponent as default, multiply as foo };
