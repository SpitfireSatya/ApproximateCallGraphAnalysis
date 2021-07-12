# ApproximateCallGraphAnalysis
QL implementation of an Approximate Call Graph Construction algorithm

Installation 
(based on detailed instructions that can be found at https://help.semmle.com/codeql/codeql-cli/procedures/get-started.html). Please note that CodeQL version >= 2.0.3 is required.
1. download the zip file for CodeQL CLI tools from https://github.com/github/codeql-cli-binaries/releases
2. create directory ~/codeql-home
3. cd ~/codeql-home/
4. git clone https://github.com/Semmle/ql.git
5. export PATH=$PATH:~/codeql-home/codeql  
6. export ANALYSIS_HOME=*<<*directory-where-you-cloned-this-project*>>*

Running the tests:
1. cd $ANALYSIS_HOME/tests
2. codeql test run --search-path=$ANALYSIS_HOME .

 


