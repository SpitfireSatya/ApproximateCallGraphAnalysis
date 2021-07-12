
var arr = ['foo', 'bar', 'baz'];

function cb(arg1, arg2) {
    console.log(arg1, arg2);
}

arr.concat([]);
arr.copyWithin(0);
arr.entries();
arr.every(s => s);
arr.fill();
arr.filter(s => s);
arr.find(s => s);
arr.findIndex(s => s);
arr.forEach(cb);
Array.from(arr);
arr.includes('foo');
arr.indexOf('foo');
Array.isArray(arr);
arr.join('.');
arr.keys();
arr.lastIndexOf('foo');
arr.map((s)=>s+s);// ['foofoo', 'barbar', 'bazbaz'];
arr.pop();
arr.push('bazbaz');
arr.reduce((accum,s)=>accum+s.length, 0);// 18
arr.reduceRight((accum,s)=>accum+s.length, 0);// 18
arr.reverse();
arr.shift();
arr.slice();
arr.some(s => s);
arr.sort();
arr.splice();
arr.toString();
arr.unshift();
arr.valueOf();