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

// flow graph edges (for unit tests, without nodes)
from MyAnalysis analysis, ASTNode a, Node source, string label, Node target 
    where analysis.flowEdge(a, source, label, target)
select source, label, target   