import javascript
import analysis

class MyAnalysis extends Analysis {
  	MyAnalysis() { // characteristic predicate
        this = TShort()
    }
    
    override 
    predicate isMainFile(File file){
    	file.getBaseName() = "test.js" 
    }
    
}

// call graph edges 
from MyAnalysis analysis, ASTNode callsite, Node sourceNode, Node targetNode, ASTNode target 
    where analysis.callGraphEdge(callsite, sourceNode, targetNode, target) 
select callsite, sourceNode, targetNode, target 