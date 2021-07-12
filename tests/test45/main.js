
const obj = { a: function() { console.log("hello"); }, b: 2, c:3 };
const { a, b } = obj;

function functionToReach() { a(); }

function test(x, { arg1 = functionToReach }) {
  arg1();
}

test("", {})
test("", { arg1: function(){ console.log("goodbye"); }});
