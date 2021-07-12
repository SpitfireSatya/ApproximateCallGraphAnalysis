import javascript
import analysis

class MyAnalysis extends Analysis {
  	MyAnalysis() { // characteristic predicate
        this = TAnalysisOption()
    }
    
    override 
    predicate isMainFile(File file){
    	file.getBaseName() = "run.js" 
    }
    
    override
    string stringRep(Node node){
    	result = node.toString()
    }
}

// call graph edges 
from MyAnalysis analysis, InvokeExpr callsite, string sourceNode, string targetNode, ASTNode target 
    where analysis.callGraphEdge(callsite, sourceNode, targetNode, target) 
select callsite, sourceNode, targetNode, target 