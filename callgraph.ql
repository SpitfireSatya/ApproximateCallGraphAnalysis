import javascript
import analysis

class MyAnalysis extends Analysis {
  	MyAnalysis() { // characteristic predicate
        this = TShort()
    }
    
    override 
    predicate isMainFile(File file){
    	file.getBaseName() = "main.js"
    }
    
}

// call graph edges (for unit tests, without nodes)
from MyAnalysis analysis, Node sourceNode, Node targetNode
    where analysis.callGraphEdge(sourceNode, targetNode) 
select sourceNode, targetNode 