
const obj = { a: function() { console.log("hello"); }, b: 2, c:3 };
const { a, b } = obj;

function functionToReach() { a(); }

function test({ arg1 = functionToReach }) {
  arg1();
}

test({});
