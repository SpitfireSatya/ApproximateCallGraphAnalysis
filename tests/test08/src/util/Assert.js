/**
 * File: util/assert.js
 */
function assert1(expected, actual){
   if (actual !== expected){
     console.log("expected " + expected + " but got " + actual);
   }
}

export const assert = assert1;
