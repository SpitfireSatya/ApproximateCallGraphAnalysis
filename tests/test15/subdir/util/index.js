/**
 * File: util/assert.js
 */
function Assert(expected, actual){
   if (actual !== expected){
     console.log("expected " + expected + " but got " + actual);
   }
}

module.exports = Assert;