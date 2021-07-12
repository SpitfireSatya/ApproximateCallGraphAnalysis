class A {
  foo(x){ console.log("foo"); }
}
class B extends A {
  bar(){ console.log("bar"); }
}

let a = new B();
a.foo();
a.bar();

