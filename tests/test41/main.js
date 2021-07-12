
const MyFunction1 = require('./my-function-1');
const MyFunction2 = require('./my-function-2');

function myMethod1() { }
function myMethod2() { }

const obj = {
  myMethod1: function() { },
  myMethod2: function() { }
};

function testFunction(arg1, arg2) {
  arg1();
  arg2();
}

const myFunction1 = new MyFunction1();
const myFunction2 = new MyFunction2();

myFunction1.myMethod1();
myFunction2.myMethod2();
