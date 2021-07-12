How to execute the tests:
-------------------------

  cd test14 // or any other test
  node --experimental-modules src/main.js

Note: need an up-to-date version of Node.js, and a file package.json needs
to be present in the directory where the command is run.


Setup:
------

export PATH=$PATH:~/codeql-home/codeql
export ANALYSIS_HOME=<directory where the analysis is located>

cd ~/codeql-home/ql
git checkout lgtm.com  // for now, the tests require the older lgtm.com version


Running the tests:
------------------
codeql test run --search-path=$ANALYSIS_HOME .
