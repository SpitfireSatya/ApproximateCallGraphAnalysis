
function test1() { console.log('test1 called') }
function test2() { console.log('test2 called') }

const object = {
  test1: test1,
  test2: test2
};

for (key in object) {
  object[key]();
}
