
import fs from 'fs';

function bar() {
  fs.stat(path, () => {
    console.log('stat callback called');
  });
}

export default function foo(path) {
  const stream = fs.createWriteStream();
  stream.on('data', () => {
    bar();
  });
}

