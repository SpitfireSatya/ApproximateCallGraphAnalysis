This is a test of code that contains flow annotations (in file util.js from the lumo project)

For some info on how to use flow, see:
	https://codeburst.io/getting-started-with-flow-and-nodejs-b8442d3d2e57
To install a tool for removing flow annotations, run:
	npm i flow-remove-types -SD

To process annotation code with CodeQL, include a file "options" containing
  semmle-extractor-options: --experimental --exclude lib/* --exclude node_modules/*
and need to create an empty file tsconfig.json

To strip away the annotations:
  npm run flow:build
(this will place the stripped version in the lib directory)

To execute the stripped code:
  node lib/main.js
  

