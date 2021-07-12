/**
 * File: util/assert.js
 */
export default function assert(expected, actual){
   if (actual !== expected){
     console.log("expected " + expected + " but got " + actual);
   }
}
