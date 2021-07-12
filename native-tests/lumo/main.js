
import fs from 'fs';
import foo from './foo';

fs.stat = jest.fn((path, cb) => {
    cb(null, { size: 10 });
});

const streamWrite = jest.fn();

fs.createWriteStream = jest.fn(() => ({
    on(_, cb) {
      cb(42);
    },
    write: streamWrite,
}));

describe('', () => {

    it('should call stat', () => {
        foo();
    });

});
