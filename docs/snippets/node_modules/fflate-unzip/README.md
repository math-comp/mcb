# fflate-unzip

A version of the `unzip` command based on fflate.
It can be used for extracting all the entries from a ZIP archive into a specified directory,
creating any required subdirectories.

The target directory can be a physical one (if running in Node.js) or
a simulated one by supplying the `fs` option to `unzip`.
The passed object must support the functions `mkdirSync`, `statSync`, and
`writeFileSync` with signatures compatible with Node.js's `fs`.
One such object can be found in [memfs](https://github.com/streamich/memfs).

```js
import unzip from 'fflate-unzip';
import fs from 'fs';

unzip(fs.readFileSync('data.zip'), {to: '/tmp/out'})
```

With `memfs`:

```js
import unzip from 'fflate-unzip';
import { fs } from 'memfs';

async function extract() {   
    var data = await fetch('data.zip'),
        raw = new Uint8Array(await data.arrayBuffer());
    await unzip(raw, {to: '/data', fs});
}
```
