var obj = {
  baz : function(){
    console.log('baz'); 
  },
  get bar(){
    console.log('bar'); 
    return this.v;
  },
  set foo(x){
    console.log('foo');
    this.v = x; 
  },
  v : undefined
}

obj.foo = obj.baz;
let b = obj.bar 
b();
