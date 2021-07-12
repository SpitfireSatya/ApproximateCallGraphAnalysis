class A {
  set foo(x){ this.f = x; }
  get bar(){ return this.f; }

  f;
}
class B {
  baz(){ console.log("done"); }
}

let a = new A();
a.foo = new B();
let b = a.bar;
b.baz();

