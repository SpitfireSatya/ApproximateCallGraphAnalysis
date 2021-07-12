/**
 * File: arraymath.js
 */

export var add = function(arr) {
                     return arr.reduce((x,y) => x+y, 0);
                 };

export let multiply = function(arr) {
                           return arr.reduce((x,y) => x*y, 1);
                      };
