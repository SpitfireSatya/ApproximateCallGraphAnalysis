/**
 * File: arraymath.js
 */

var add = function(arr) {
    return arr.reduce((x,y) => x+y, 0);
};
var multiply = function(arr) {
    return arr.reduce((x,y) => x*y, 1);
};

export { add as ADD, multiply as MULT };
