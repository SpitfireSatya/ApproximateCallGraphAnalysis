Note: need to specify "--es-module-specifier-resolution=node" to ensure that the
      ".js" file extension can be omitted when importing files.

Hence, to run this test:

  node --es-module-specifier-resolution=node --experimental-modules main.js
