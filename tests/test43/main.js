
const obj = { a: function() {}, b: 2, c:3 };
const { a, b } = obj;

function functionToReach() { }

function test({ arg1 = functionToReach }) {
  arg1();
}

test({});
