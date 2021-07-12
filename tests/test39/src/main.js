import { currentTimeMicros } from './util.js'
import { foo } from './foo.js'

foo();
console.log(currentTimeMicros());
