/**
 * File: util/assert.js
 */
function Assert(expected, actual){
   if (actual !== expected){
     console.log("expected " + expected + " but got " + actual);
   }
}

function unreachable2(){
  var x = 10;
}

var y = function(){ /* unreachable */ }

module.exports = Assert;
