class A {
  foo(b){ b.bar(this); }
  baz(){ }
}
class B {
  bar(a){ a.baz();  }
}

let a = new A();
let b = new B();
a.foo(b);


