
const bar = require('../../src2');

function baz() {
  bar();
  console.log('baz called');
}

module.exports = baz;
