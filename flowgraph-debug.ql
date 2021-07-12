import javascript
import analysis

class MyAnalysis extends Analysis {
  	MyAnalysis() { // characteristic predicate
        this = TLong()
    }
    
    override 
    predicate isMainFile(File file){
    	file.getBaseName() = "main.js" 
    }
}

// flow graph edges
from MyAnalysis analysis, ASTNode a, Node source, string label, Node target
    where analysis.flowEdge(a, source, label, target)
select a.getFile(), a, source, label, target