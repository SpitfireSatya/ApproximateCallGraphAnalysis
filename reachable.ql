import javascript
import analysis

class MyAnalysis extends Analysis {
  	MyAnalysis() { 
        this = TShort() // short source locations
    }
    
    override 
    predicate isMainFile(File file){
    	file.getBaseName() = "main.js" 
    }
}

bindingset[s]
string getExternPath(string s){
  if (s.indexOf("externs") > 0) then
    result = s.suffix(s.indexOf("externs"))
  else
    result = s
}

// reachable files
from MyAnalysis analysis, File s 
  where analysis.reachable(s)
select getExternPath(s.getAbsolutePath())
