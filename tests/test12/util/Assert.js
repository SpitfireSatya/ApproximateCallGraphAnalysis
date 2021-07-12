/**
 * File: util/assert.js
 */

function unreachable8(){
  var x = function unreachable9(){ }
}

export default function assert(expected, actual){
   if (actual !== expected){
     console.log("expected " + expected + " but got " + actual);
   }
}
