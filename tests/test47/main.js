function test(x, { arg1 }) {
  arg1();
}

test("", { arg1: function(){ console.log("goodbye"); }});
