
function functionToReach() { }

function intermediateFunction(arg1) { 
  arg1();
}

function intermediateWithCallback(callback) {
  if(callback) {
    callback();
  }
}

function test(arg1 = functionToReach) {

  intermediateWithCallback(() => {
    intermediateFunction(arg1);
  });

}

test();
