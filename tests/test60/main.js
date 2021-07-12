class A {
  set foo(x){ this.f = x; }
  f;
}

let a = new A();
a.foo = function(){}

