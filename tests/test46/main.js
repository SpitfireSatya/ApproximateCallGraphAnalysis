function test(x, { arg1 = function(){} }) {
  arg1();
}

test("", { arg1: function(){ console.log("goodbye"); }});
