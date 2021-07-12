var obj = {
 
  foo : function(){ console.log('foo'); },
  get bar(){
    console.log('bar');
    return function(){ console.log('baz'); }
  }
}

obj.foo();
let baz = obj.bar;
baz();
