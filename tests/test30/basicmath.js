/**
 * file: basicmath.js
 */

class B {

  constructor(){ }

  multiply(frst, second){
	return frst * second;
  }

  exponent(base, exp){
	return Math.pow(base, exp);
  }

}

export { B as BasicMath }
