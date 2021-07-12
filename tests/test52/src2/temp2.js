
const foo = require('../src1/temp1');

function bar() {
  foo();
  console.log('bar called');
}

module.exports = bar;
