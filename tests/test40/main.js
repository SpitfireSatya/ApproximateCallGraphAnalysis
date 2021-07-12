
const MyClass = require('./my-class');

const obj = {
  myMethod: function myMethod() { }
};

const obj2 = {
  myMethod: function myMethod() { }
};

function testFunction(arg1) {
  arg1();
}

const myClass = new MyClass();
testFunction(myClass.myMethod);
