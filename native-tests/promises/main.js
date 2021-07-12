/**
* File: main.js
*/

function foo(){ }
function bar(){ baz(); }
function baz(){ }

var p = new Promise((resolve,reject) => {
   foo();
   bar();
   resolve(17);
});

p.then((v)=>{ console.log(v); });
