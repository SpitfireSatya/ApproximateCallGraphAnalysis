import javascript
import semmle.javascript.Externs

predicate isCallToRequire(InvokeExpr ie){
	ie.getCalleeName() = "require" 
}

// need to make sure that an ModuleExports node is created even if the entire object is not exported itself
predicate isModuleExport(PropAccess pa){
	pa.getPropertyName() = "exports" /*and not exists(PropAccess pa2 | pa2.getBase() = pa)*/
}

predicate isPropertyExport(PropAccess pa){
	(pa.getBase() instanceof PropAccess and ((PropAccess)pa.getBase()).getPropertyName() = "exports")
}

predicate getExportedNames(BulkImportDeclaration bid, string name){
	exists(File file, ExportNamedDeclaration end |  
		file = any(Analysis analysis).getImportedFile(bid) and
		end.getFile() = file and
		name = end.getASpecifier().getExportedName()
	)
} 

/** 
 * Maps function expressions to the surrounding constructor/method/getter/setter if applicable. Returns
 * the function node itself for function definitions and function expressions that are not part of 
 * constructor/method/getter/setter.
 */
ASTNode getCompleteFunction(Function fun){
	if (fun.getParent() instanceof MethodDeclaration) or 
	   (fun.getParent() instanceof GetterMethodDefinition) or 
	   (fun.getParent() instanceof SetterMethodDefinition) or 
	   (fun.getParent() instanceof PropertyGetter) or 
	   (fun.getParent() instanceof PropertySetter) then
	   result = fun.getParent()
	 else
	   result = fun
}

predicate isReachable(ASTNode node){
	any(Analysis analysis).reachable(node.getFile())
}

newtype TNode =
	  ArgNode(Expr arg) { isReachable(arg) and arg = any(InvokeExpr ie).getAnArgument() } or
	  ReceiverNode(Expr a){ isReachable(a) and (a instanceof MethodCallExpr or a instanceof PropAccess) } or                                      
	  CalleeNode(Expr a){ isReachable(a) and (a instanceof InvokeExpr or a instanceof PropAccess) }  or
	  ClassDeclNode(ClassDeclStmt cds){ isReachable(cds) } or
	  ExpNode(Expr exp){ isReachable(exp) } or
	  CommonJSExportModNode(TopLevel top) {  exists(PropAccess pa | isReachable(top) and isModuleExport(pa) and top = pa.getTopLevel() ) } or
		ESMExportModNode(TopLevel top) { exists(ExportDeclaration ed | isReachable(ed) and top = ed.getTopLevel() ) } or 							   
		CommonJSExportNameNode(PropAccess node){  isReachable(node) and isPropertyExport(node) } or
		ESMExportNameNode(File file, string name){ exists(ExportDeclaration ed | isReachable(ed) and file = ed.getFile() 
			            and (ed.exportsAs(_, name) or name = ((ExportNamedDeclaration)ed).getASpecifier().getExportedName()) ) }
		or
	  FunNode(Function fun){ isReachable(fun) } 
	  or
	  ParamNode(Parameter param){ isReachable(param) and param = any(Function f).getAParameter() }
	  or
	  ThisNode(Function fun){ isReachable(fun) }   
	  or 
	  PropNode(string name) { exists(PropAccess pa | isReachable(pa) and name = pa.getPropertyName() ) or 
	                          exists(ObjectPattern op | isReachable(op) and name = op.getABindingVarRef().getVariable().getName())
	  } 
	  or
	  CommonJSImportModNode(InvokeExpr ie){ isReachable(ie) and isCallToRequire(ie) and exists(File file, Analysis analysis | file = analysis.getRequiredFile(ie))}   
	  or
	  ESMImportModNode(ImportDeclaration id){ isReachable(id) and exists(File file, Analysis analysis | file = analysis.getImportedFile(id)) }
	  or
		ESMImportNameNode(ImportDeclaration id, string name){ isReachable(id) and 
			(exists(BulkImportDeclaration bid, File file, ExportDeclaration ed | id = bid and 
         file = any(Analysis analysis).getImportedFile(bid) and ed.getFile() = file and ed.exportsAs(_, name) ) 
			or
      (exists(BulkImportDeclaration bid, File file, PropAccess pa | id = bid and 
				file = any(Analysis analysis).getImportedFile(bid) and pa.getFile() = file and isPropertyExport(pa) and 
				name = pa.getPropertyName() )) 
		 or
			name = id.getASpecifier().getImported().getName())  
		}
		or
		CommonJSImportNameNode(InvokeExpr ie, string name){ 
			isCallToRequire(ie) and 
			(exists(PropAccess pa | isPropertyExport(pa) and pa.getFile() = any(Analysis analysis).getRequiredFile(ie) 
																									 and name = pa.getPropertyName() )) 
			or
			(exists(PropAccess pa | pa.getBase() = ie and  name = "default" and name = pa.getPropertyName())) 
		} 
		or
		ResNode(Expr a){ isReachable(a) and (a instanceof InvokeExpr  or a instanceof PropAccess) } or
	  RetNode(Function fun){ isReachable(fun) } or
	  VarNode(VarDecl vd){ isReachable(vd) }    
 
class Node extends TNode {
		
  private string formatLocation(ASTNode node) {  
	isReachable(node) and
	exists(Analysis analysis, string loc | loc = ":" + "<" + node.getLocation().getStartLine() + "," + node.getLocation().getStartColumn() + ">--" +
							        "<" + node.getLocation().getEndLine() + "," + node.getLocation().getEndColumn() + ">" 
					    and 
	                    ((analysis = TShort() and result = node.getLocation().getFile().getBaseName() + loc )
	                     or         
						 (analysis = TLong() and result = node.getLocation().getFile().getAbsolutePath() + loc ))
	)
  }
	
  string toString() {
    exists ( InvokeExpr ie, Expr arg, int i | isReachable(ie) and isReachable(arg) and
       this = ArgNode(arg) and
       ie.getArgument(i) = arg and
       result = "Arg(" + formatLocation(ie) + "," + (i+1) + ")"  // add 1, because argument 0 is the receiver
    )
  	or
  	exists ( MethodCallExpr mce |  this = ReceiverNode(mce)  and result = "Receiver(" + formatLocation(mce) + ")" )
  	or
  	exists ( PropAccess pa |  this = ReceiverNode(pa)  and result = "Receiver(" + formatLocation(pa) + ")" )
  	or
  	exists ( InvokeExpr ie |  this = CalleeNode(ie) and result = "Callee(" + formatLocation(ie) + ")" )
  	or
  	exists ( PropAccess pa |  this = CalleeNode(pa) and result = "Callee(" + formatLocation(pa) + ")" )
  	or	
  	exists ( ClassDeclStmt cds  | this = ClassDeclNode(cds) and result = "Class(" + formatLocation(cds) + ")" )
    or
    exists ( Expr exp | this = ExpNode(exp)  and result = "Exp(" + formatLocation(exp) + ")" )
    or 
    exists ( TopLevel top | this = CommonJSExportModNode(top)   and result = "ModuleExport(" + top.getFile().getBaseName() + ")" )
	  or 
	  exists ( TopLevel top | this = ESMExportModNode(top)   and result = "ModuleExport(" + top.getFile().getBaseName() + ")" )
    or 
    exists ( PropAccess pa, File file, string name | this = CommonJSExportNameNode(pa) and file = pa.getFile()  
		and name = pa.getPropertyName() and result = "ExportName(" +  file.getBaseName() + "," + name + ")" )    
		or 
    exists ( File file, string name | this = ESMExportNameNode(file, name)  and result = "ExportName(" +  file.getBaseName() + "," + name + ")" )    
		or
    exists ( Function fun | this = FunNode(fun)  and result = "Fun(" + formatLocation(getCompleteFunction(fun)) + ")")
    or 
    exists ( Function fun, Parameter param, int i | this = ParamNode(param) and fun.getParameter(i) = param and
         result = "Param(" + formatLocation(getCompleteFunction(fun)) + "," + (i+1) + ")"  // add 1, because parameter 0 is the receiver
    )
    or
    exists ( Function fun  | this = ThisNode(fun)  and result = "This(" + formatLocation(fun) + ")" )
    or
    exists ( string name  | this = PropNode(name)  and result = "Prop(" + name + ")" )
    or
    exists ( InvokeExpr ie | this = CommonJSImportModNode(ie) and result = "ImportMod(" + formatLocation(ie) + "," + any(Analysis analysis).getRequiredFile(ie).getBaseName() + ")" )
	or
    exists ( ImportDeclaration id | this =ESMImportModNode(id) and result = "ImportMod(" + formatLocation(id) + "," + any(Analysis analysis).getImportedFile(id).getBaseName() + ")" )
	or
    exists ( ImportDeclaration id, string name | this = ESMImportNameNode(id, name) and result = "ImportName(" + formatLocation(id) + "," + name + ")" )
    or 
    exists ( InvokeExpr ie, string name | this = CommonJSImportNameNode(ie, name)  and result = "ImportName(" + formatLocation(ie) + "," + name + ")" )
    or
    exists ( ASTNode a | this = ResNode(a)  and result = "Res(" + formatLocation(a) + ")" )
    or
    exists ( Function fun | this = RetNode(fun) and result = "Ret(" + formatLocation(getCompleteFunction(fun)) + ")" )
    or
    exists ( VarDecl vd | this = VarNode(vd)  and result = "Var(" + formatLocation(vd) + ")" )
  }
}	

newtype TAnalysis =
  TShort() or
  TLong()

abstract class Analysis extends TAnalysis {
	 
	Analysis() {  
        this = TShort() or this = TLong()
    }
    
    string toString(){ // required if we want to do queries over this type
        result = "AbstractAnalysis"	
	}	
	/**
	* Find the file referenced in an "import" statement. We do this by first computing the absolute path of the
	* referenced file, and then finding a File object corresponding to that path. 
	*/
   File getImportedFile(ImportDeclaration id){ 
	   reachable(id.getFile())
	   and
	   (exists(string importPath, string prefix, File file |
		  importPath = id.getImportedPath().getValue() and
		  prefix = id.getFile().getAbsolutePath().substring(0, id.getFile().getAbsolutePath().indexOf(id.getFile().getBaseName())) and
		  ( file.getAbsolutePath() = importPath.replaceAll("./", prefix) or
			file.getAbsolutePath() = importPath.replaceAll("./", prefix) + ".js")  and
			result = file
	   )
	   or
	   exists(string importPath, string prefix, Container parent, File file |
			  importPath = id.getImportedPath().getValue() and
		  parent = id.getFile().getParentContainer().getParentContainer() and
			  prefix = parent.getAbsolutePath() and
			  ( file.getAbsolutePath() = importPath.replaceAll("../", prefix + "/") or
				file.getAbsolutePath() = importPath.replaceAll("../", prefix + "/") + ".js") and
			  result = file 
		)
	   or
	   // import externs
	   exists(string importPath, string prefix, File file |
		   importPath = id.getImportedPath().getValue() and
		   prefix = id.getImportedPath().getValue() and
		   file.getAbsolutePath().toString().indexOf("/externs/nodejs/" + prefix + ".js") != -1  and
		   result = file
	))
   }
   
   File getReExportedFile(ReExportDeclaration be){
	   reachable(be.getFile())
	   and
	   exists(string importPath, string prefix, File file |
		  importPath = be.getImportedPath().toString() and
		  prefix = be.getFile().getAbsolutePath().substring(0, be.getFile().getAbsolutePath().indexOf(be.getFile().getBaseName())) and
		  (file.getAbsolutePath() = importPath.replaceAll("./", prefix).substring(1,importPath.replaceAll("./", prefix).length()-1) or 
		   file.getAbsolutePath() = importPath.replaceAll("./", prefix).substring(1,importPath.replaceAll("./", prefix).length()-1) + ".js"
		  ) and
		  result = file
	   )
   }
   
   /**
	* Find the file referenced in a "require" expression. We do this by first computing the absolute path of the
	* referenced file, and then finding a File object corresponding to that path. 
	*/
   File getRequiredFile(InvokeExpr ie){
	   exists(string importPath| 
		   reachable(ie.getFile()) and
		   importPath = ((StringLiteral)ie.getChild(0)).getStringValue() and
		   (
		   // require a .js file
		   exists(string prefix, File file | 
			   prefix = ie.getFile().getAbsolutePath().substring(0, ie.getFile().getAbsolutePath().indexOf(ie.getFile().getBaseName())) and
			   file.getAbsolutePath() = importPath.replaceAll("./", prefix) and
			   result = file
		   )
		   or
		   // require a .js file without providing the extension
		   exists(string prefix, File file | 
			   prefix = ie.getFile().getAbsolutePath().substring(0, ie.getFile().getAbsolutePath().indexOf(ie.getFile().getBaseName())) and
			   file.getAbsolutePath() = importPath.replaceAll("./", prefix) + ".js" and
			   result = file 
		   )
		   or
		   // require a .js file relative to ".." 
		   exists(string prefix, Container parent, File file |
			   parent = ie.getFile().getParentContainer().getParentContainer() and 
			   prefix = parent.getAbsolutePath() and
			   file.getAbsolutePath() = importPath.replaceAll("../", prefix + "/") and
			   result = file 
		   )
		   or
		   // require a .js file relative to "../.."  
		   exists(string prefix, Container parent, File file |
			   parent = ie.getFile().getParentContainer().getParentContainer().getParentContainer() and 
			   prefix = parent.getAbsolutePath() and
			   file.getAbsolutePath() = importPath.replaceAll("../../", prefix + "/") and
			   result = file 
		   )
		   or
		   // require a .js file relative to ".." without providing the extension
		   exists(string prefix, Container parent, File file |
			   parent = ie.getFile().getParentContainer().getParentContainer() and 
			   prefix = parent.getAbsolutePath() and
			   file.getAbsolutePath() = importPath.replaceAll("../", prefix + "/") + ".js" and
			   result = file 
		   )
		   or
		   // require an NPM package relative to "."
		   exists(NPMPackage pkg, Container parent, PackageJSON pj, File file |
			   parent = ie.getFile().getParentContainer() and 
			   importPath.indexOf("./") = 0 and
			   pj = pkg.getPackageJSON() and
			   pj.getFile().getAbsolutePath() = parent.getAbsolutePath() + "/" + importPath.substring(2, importPath.length()) + "/package.json" and
			   file = pkg.getAFile() and 
			   file.getAbsolutePath() = pj.getFile().getParentContainer().getAbsolutePath() + "/" + pj.getMain() and
			   result = file 
		   )
		   or
		   // require an NPM package relative to ".."
		   exists(NPMPackage pkg, Container parent, PackageJSON pj, File file |
			   parent = ie.getFile().getParentContainer().getParentContainer() and 
			   importPath.indexOf("../") = 0 and
			   pj = pkg.getPackageJSON() and
			   pj.getFile().getAbsolutePath() = parent.getAbsolutePath() + "/" + importPath.substring(3, importPath.length()) + "/package.json" and
			   file = pkg.getAFile() and 
			   file.getAbsolutePath() = pj.getFile().getParentContainer().getAbsolutePath() + "/" + pj.getMain() and
			   result = file 
		   )
		   or
		   // require an NPM package relative to "../.."
		   exists(NPMPackage pkg, Container parent, PackageJSON pj, File file |
			   parent = ie.getFile().getParentContainer().getParentContainer().getParentContainer() and 
			   importPath.indexOf("../../") = 0 and
			   pj = pkg.getPackageJSON() and
			   pj.getFile().getAbsolutePath() = parent.getAbsolutePath() + "/" + importPath.substring(6, importPath.length()) + "/package.json" and
			   file = pkg.getAFile() and 
			   file.getAbsolutePath() = pj.getFile().getParentContainer().getAbsolutePath() + "/" + pj.getMain() and
			   result = file 
		   )
		   or
		   // require a .js file relative to "../.." without providing the extension
		   exists(string prefix, Container parent, File file |
			   parent = ie.getFile().getParentContainer().getParentContainer().getParentContainer() and 
			   prefix = parent.getAbsolutePath() and
			   file.getAbsolutePath() = importPath.replaceAll("../../", prefix + "/") + ".js" and
			   result = file 
		   )
		   or
		   // require a directory relative to "."; in this case we look for an index.js file within that directory
		   exists(string prefix, File file | 
			   prefix = ie.getFile().getAbsolutePath().substring(0, ie.getFile().getAbsolutePath().indexOf(ie.getFile().getBaseName())) and
			   file.getAbsolutePath().indexOf(importPath.replaceAll("./", prefix)) = 0 and 
			   file.getBaseName() = "index.js" and
			   result = file 
		   )
		   or
		   // require a directory relative to ".."; in this case we look for an index.js file within that directory
		   exists(string prefix, Container parent, File file |
			   parent = ie.getFile().getParentContainer().getParentContainer() and 
			   prefix = parent.getAbsolutePath() and
			   file.getAbsolutePath().indexOf(importPath.replaceAll("../", prefix + "/")) = 0 and 
			   file.getBaseName() = "index.js" and
			   result = file 
		   )
		   or
		   // require externs
		   exists(string prefix, File file | 
			   prefix = ((StringLiteral)ie.getChild(0)).getStringValue() and
			   file.getAbsolutePath().toString().indexOf("/externs/nodejs/" + prefix + ".js") != -1 and
			   result = file 
		   )
		   ))
   }
			
	/**
	 * Construct the flow/call graph node corresponding to an AST Node.
	 */
	Node v(ASTNode a){
		reachableASTNode(a) 
		and
		if a instanceof VarRef then 
		    if (exists(Parameter param | param.getAVariable() = ((VarRef)a).getVariable())) then
		        exists(Parameter param | param.getAVariable() = ((VarRef)a).getVariable() and result = ParamNode(param) )	       
			else
			    result = VarNode(((VarRef)a).getVariable().getADeclaration())
		else if a instanceof PropAccess then
		   if isPropertyExport(a) then
		     result = CommonJSExportNameNode(((PropAccess)a))
		   else if isModuleExport(a) then
		     result = CommonJSExportModNode(a.getTopLevel())
			 else 
				 exists(PropAccess pa, string name |
					 pa = ((PropAccess)a) and
					 name = pa.getPropertyName() and
					 (result = PropNode(name) or 
					  exists(InvokeExpr ie | ie.getTopLevel() = pa.getTopLevel() and result = CommonJSImportNameNode(ie, name)) or
					  exists(BulkImportDeclaration id | id.getTopLevel() = pa.getTopLevel() and result = ESMImportNameNode(id, name))
					 )
				 )
		else if a instanceof InvokeExpr and not isCallToRequire(a) and not isBind(a) then // avoid spurious call graph edges for bind()			 
			result = CalleeNode(a) 
	    else if a instanceof InvokeExpr and isCallToRequire(a) then
	        result = CommonJSImportModNode((InvokeExpr)a)
		else if a instanceof FunctionExpr or a instanceof ArrowFunctionExpr then
			result = FunNode(a)                  
		else if a instanceof ThisExpr then
		    result = ThisNode(((ThisExpr)a).getBinder())
		else if a instanceof SuperExpr then
		    result = ThisNode(((SuperExpr)a).getBinder())
		else  
		    result = ExpNode(a) 
	}
	
	// ----------------------------------------------------------------------------------- //
		
	/**
	 * generate flow edges for assignments l = r
	 */
	predicate rAssign1(Assignment ass, Node source, string label, Node target){ 
		source = v(ass.getRhs()) and
	      label = "[Assign-1]" and
		  target = v(ass.getLhs())
	}
		
	predicate rAssign2(Assignment ass, Node source, string label, ExpNode target){ 	
		source = v(ass.getRhs()) and
	    label = "[Assign-2]" and
	    target = ExpNode(ass) 
	}

	predicate rAssign3(VariableDeclarator vd, Node source, string label, Node target){ 
		source = v(vd.getInit()) and
	    label = "[Assign-3]" and
	    target = VarNode(vd.getBindingPattern().getABindingVarRef().getVariable().getADeclaration()) //v(vd.getBindingPattern().getABindingVarRef())
	}

	predicate rAssign4(Parameter param, Node source, string label, Node target){ 
		source = v(param.getDefault()) and // rhs
		label = "[Assign-4]" and
		target = v(param)
	}

	predicate rAssign5(VarRef lhs, PropNode source, string label, Node target){ 
		exists (PropertyPattern pp |
			   lhs.getParent() = pp and
			   lhs.isLValue() and 
			   source = PropNode(pp.getName()) and
			   label = "[Assign-5]" and
			   target = v(lhs)
		)
	}
	
	predicate rAssign6(VarRef lhs, Node source, string label, PropNode target){ 
		exists (PropertyPattern pp |
			   lhs.getParent() = pp and
			   lhs.isLValue() and
			   source = v(pp.getDefault()) and // rhs
			   label = "[Assign-6]" and
			   target = PropNode(pp.getName())
		)
	}

	/**
	 * generate flow edges for destructuring assignments
	 */
	predicate rDestructuringAssign1(VariableDeclarator vd, Node source, string label, PropNode target){ 
		exists(ObjectPattern lhs, Expr rhs, VarRef vr |
			lhs = vd.getBindingPattern() and
			rhs = vd.getInit() and
			vr = lhs.getABindingVarRef() and
			source = v(vr) and
			label = "[DestructuringAssign-1]" and
			target = PropNode(vr.getVariable().getName())
		)
	}
		
	predicate rDestructuringAssign2(VariableDeclarator vd, PropNode source, string label, VarNode target){ 
		exists(ObjectPattern lhs, VarRef vr |
			lhs = vd.getBindingPattern() and 
			vr = lhs.getABindingVarRef() and
			source = PropNode(vr.getVariable().getName()) and
			label = "[DestructuringAssign-2]" and
			target = VarNode(vr)
		)
	}

	/**
	 * generate flow edges for conditional expressions t ? l : r
	 */
	predicate rConditionalExpr(ConditionalExpr ce, Node source, string label, ExpNode target){ 
	    (source = v(ce.getConsequent()) and label = "[ConditionalExpr-1]" and target = ExpNode(ce))
	    or
	    (source = v(ce.getAlternate()) and label = "[ConditionalExpr-2]" and target = ExpNode(ce))
	}	
	
	/**
	 * generate flow edges for logical-and expressions l && r
	 */
	predicate rLogicalAnd(LogAndExpr le, Node source, string label, ExpNode target){ 
	  	source = v(le.getRightOperand()) and
	    label = "[LogicalAnd]" and
	    target = ExpNode(le)      
	}	
	
	/**
	 * generate flow edges for logical-or expressions l || r
	 */
	predicate rLogicalOr(LogOrExpr le, Node source, string label, ExpNode target){ 
		(source = v(le.getLeftOperand()) and label = "[LogicalOr-1] " and target = ExpNode(le)) 
	    or
	    (source = v(le.getRightOperand()) and label = "LogicalOr-2] " and target = ExpNode(le))
	}
	
	/**
	 * generate flow edges for parenthesized expressions (e)
	 */
	predicate rParenthesizedExpr(ParenthesizedExpr pe, Node source, string label, Node target){ 
		label = "[ParenthesizedExpr]" and 
		source = v(pe.getExpression()) and
		target = v(pe) 
	}	
	
	/**
	 * generate flow edges for object literals { f_1:e_1, ..., f_2:e_n }
	 */
	predicate rObjLiteral(ObjectExpr oe, Node source, string label, PropNode target){ 
	    exists(int i, Property prop | 
		   0 <= i and i < oe.getNumProperty() and 
		   prop = oe.getProperty(i) and  
		   source = v(prop.getInit()) and
		   label = "[ObjLiteral]" and
		   target = PropNode(prop.getName())
		)
	}
	
	/**
	 * generate flow edges for function expressions function f(){ ... }
	 */
	predicate rFunctionExpr1(FunctionExpr fe, FunNode source, string label, ExpNode target){ 
		not fe.getParent() instanceof ConstructorDefinition and
		not fe.getParent() instanceof MethodDefinition and
		source = FunNode(fe) and
		label = "[FunctionExpr-1]" and
		target = ExpNode(fe) 
	}
		
	predicate rFunctionExpr2(FunctionExpr fe, FunNode source, string label, VarNode target){ 
		source = FunNode(fe) and
		label = "[FunctionExpr-3]" and
		target = VarNode(fe.getId())
	}

	predicate rArrowFunctionExpr(ArrowFunctionExpr fe, FunNode source, string label, ExpNode target){ 
		source = FunNode(fe) and
		label = "[FunctionExpr-2]" and
		target = ExpNode(fe) 
	}
	
	/**
	 * generate flow edges for function declarations function f(){ ... }
	 */
	predicate rFunctionDecl(FunctionDeclStmt fd, FunNode source, string label, VarNode target){ 
		source = FunNode(fd) and
		label = "[FunctionDecl]" and
		target = VarNode(fd.getId())
	}
	
	/**
	 * generate flow edges for function calls f(e_1,...,e_n) or new f(e_1, ..., e_n)
	 */
	predicate rFunctionCall1(InvokeExpr ie, Node source, string label, CalleeNode target){ 
		not isCallToRequire(ie) and 
		source = v(ie.getCallee()) and 
		label = "[FunctionCall-1]" and 
		target = CalleeNode(ie) 
	}

	predicate rFunctionCall2(InvokeExpr ie, Node source, string label, ArgNode target){ 
		exists (Expr arg, int numArgs, int i |   
		    not isCallToRequire(ie) and 
		    numArgs = ie.getNumArgument() and 
		    0 <= i and i <= numArgs and 
		    arg = ie.getArgument(i) and 
		    source = v(arg) and
		    label = "[FunctionCall-2]" and
		    target = ArgNode(arg)
		) 
	}

	predicate rFunctionCall3(InvokeExpr ie, ResNode source, string label, ExpNode target){ 
		source = ResNode(ie) and 
		target = ExpNode(ie) and 
		label = "[FunctionCall-3]"
	}
	
	/**
	 * generate edges for super-calls super(..) in constructors
	 */
	predicate rSuperCall(InvokeExpr ie, Node source, string label, CalleeNode target){
		exists(ClassDeclStmt cds, ClassDeclStmt supercds, ConstructorDefinition cd, ConstructorDefinition supercd, Function supercdfun  |
			ie.getCallee() instanceof SuperExpr and
			cd.getDeclaringClass() = cds and
			cds.getSuperClassDefinition() = supercds and
			supercd.getDeclaringClass() = supercds and
			supercdfun.getParent() = supercd and
			source = FunNode(supercdfun) and
			target = CalleeNode(ie) and
			label = "[SuperCall]" 
		)
	}
	
	/**
	 * generate flow edges for method calls r.p(e_1,...,e_n)  
	 */
	predicate rFunctionCallReceiver(MethodCallExpr me, Node source, string label, ReceiverNode target){
		source = v(me.getReceiver()) and label = "[FunctionCall-Receiver]" and target = ReceiverNode(me)
	}
	
	/**
	 * generate flow edges for return statements return e 
	 */
	predicate rReturn(ReturnStmt ret, Node source, string label, RetNode target){
		source = v(ret.getExpr()) and
		label = "[Return]" and
		target = RetNode(ret.getTarget()) 
	}
	
	/**
	 * given flow from FunNode -> CalleeNode, create corresponding edges ArgNode -> ParamNode 
	 */ 
	predicate rArgToParam1(Function fun, ArgNode argNode, string label, ParamNode paramNode){ 
	    exists(FunNode funNode, CalleeNode calleeNode, InvokeExpr ie, Expr arg, int i | 
	    	funNode = FunNode(fun) and 
	    	calleeNode = CalleeNode(ie) and not isCall(ie) and
	        flowPath(funNode, calleeNode) and
	    	arg = ie.getArgument(i) and
	  	    argNode = ArgNode(arg) and
	    	0 <= i and i < ie.getNumArgument() and 
	        paramNode = ParamNode(fun.getParameter(i)) and 
	    	label = "[ArgToParam-1]"
		)
	}
	
	/**
	 * given flow from Fun(f) -> Callee(callSite), create corresponding edges ReceiverNode(callSite) -> This(f) 
	 */    
	predicate rArgToParam2(Function fun, ReceiverNode recNode, string label, ThisNode thisNode){  
	    exists( FunNode funNode, CalleeNode calleeNode, MethodCallExpr mce | 
	    	funNode = FunNode(fun) and 
	        flowPath(funNode, calleeNode) and
	        calleeNode = CalleeNode(mce) and not isCall(mce) and
	        recNode = ReceiverNode(mce) and 
	    	thisNode = ThisNode(fun) and
	    	label = "[ArgToParam-2]"
	    )
	}
	
    /**
	 * given flow from Fun(f) -> Callee(callSite), where callSite is a call of the form E.call(...),
	 *  create edges from arguments 2,...,N+1 to parameters 1,...,N
	 */
	predicate rCallArgToParam1(Function fun, ArgNode argNode, string label, ParamNode paramNode){ 
		exists(FunNode funNode, CalleeNode calleeNode, InvokeExpr ie, Expr arg, int i | 
	    	funNode = FunNode(fun) and 
	    	calleeNode = CalleeNode(ie) and isCall(ie) and
	        flowPath(funNode, calleeNode) and
	    	arg = ie.getArgument(i) and
	  	    argNode = ArgNode(arg) and
	    	1 <= i and i < ie.getNumArgument() and 
	        paramNode = ParamNode(fun.getParameter(i-1)) and 
	    	label = "[CallArgToParam-1]"
		)
	}

	/**
	 * given flow from Fun(f) -> Callee(callSite), where callSite is a call of the form E.call(...),
	 *  create edges from argument 1 to the function's ThisNode
	 */
	predicate rCallArgToParam2(Function fun, ArgNode argNode, string label, ThisNode paramNode){ 
		exists(FunNode funNode, CalleeNode calleeNode, InvokeExpr ie | 
			funNode = FunNode(fun) and 
			flowPath(funNode, calleeNode) and 
	    	calleeNode = CalleeNode(ie) and isCall(ie) and
	  	    argNode = ArgNode(ie.getArgument(0)) and 
	        paramNode = ThisNode(fun) and 
	    	label = "[CallArgToParam-2]"
		)
	}

	/**
	 * given flow from Fun(f) -> Callee(callSite), where callSite is a call of the form E.apply(...),
	 *  create edges from argument 1 to the function's ThisNode
	 */
	predicate rApplyArgToParam(Function fun, ArgNode argNode, string label, ThisNode paramNode){ 
		exists(FunNode funNode, CalleeNode calleeNode, InvokeExpr ie | 
			funNode = FunNode(fun) and 
			flowPath(funNode, calleeNode) and 
	    	calleeNode = CalleeNode(ie) and isApply(ie) and
	  	    argNode = ArgNode(ie.getArgument(0)) and 
	        paramNode = ThisNode(fun) and 
	    	label = "[ApplyArgToParam]"
		)
	}

	/**
	 * given flow from Fun(f) -> Callee(callSite), where callSite is a call of the form E.bind(...),
	 *  create edges from argument 1 to the function's ThisNode
	 */
	predicate rBindArgToParam(Function fun, ArgNode argNode, string label, ThisNode paramNode){ 
		exists(FunNode funNode, CalleeNode calleeNode, InvokeExpr ie | 
			funNode = FunNode(fun) and 
	        flowPath(funNode, calleeNode) and 
	    	calleeNode = CalleeNode(ie) and isBind(ie) and
	  	    argNode = ArgNode(ie.getArgument(0)) and 
	        paramNode = ThisNode(fun) and 
	    	label = "[BindArgToParam]"
		)
	}
	
	/**
     *  given flow from FunNode -> CalleeNode, create corresponding edge RetNode -> ResNode
     */
	predicate rRetToRes(Function fun, RetNode retNode, string label, ResNode resNode){ 
		not fun.getParent() instanceof ConstructorDefinition and
		exists(FunNode funNode, CalleeNode calleeNode, Expr ie | 
	    	funNode = FunNode(fun) and 
	        flowPath(funNode, calleeNode) and
	        calleeNode = CalleeNode(ie) and
	        resNode = ResNode(ie) and 
	   	    retNode = RetNode(fun) and
	    	label = "[RetToRes]"
	    )
	}
	
	/**
	 * generate flow edges for class declarations class X { ... }
	 */
	predicate rClassDecl(ClassDeclStmt cds, Node source, string label, VarNode target){ 
		source = ClassDeclNode(cds) and
		label = "[ClassDecl]" and
		target = VarNode(cds.getIdentifier()) 
	}

	/**
	 * generate flow edges for method declarations in class C { ... md(){ ... } ... } 
	 */
	predicate rMethodDecl(MethodDeclaration md, FunNode source, string label, PropNode target){ 
		source = FunNode(md.getInit()) and
		label = "[MethodDecl]" and
		target = PropNode(md.getName())
	}
	
	/**
	 * generate flow edges for constructor calls new X(), where X is a class
	 */
	predicate rCtorCall(InvokeExpr ctorCall, FunNode source, string label, CalleeNode target){ 
		exists (ClassDeclStmt cd, MethodDeclaration ctorDecl, ClassDeclNode cdNode | 
		    cd.getAMethod() = ctorDecl and ctorDecl.getName() = "constructor" and
		    source = FunNode(ctorDecl.getBody()) and
		    cdNode = ClassDeclNode(cd) and
		    label = "[CtorCall]" and
		    flowPath(cdNode, target)  and
		    target = CalleeNode(ctorCall)
		)
	}
		
	predicate hasMethod(ClassDefinition cd, MethodDeclaration md){
		reachable(cd.getFile()) and
		(md = cd.getAMethod()
		or
		exists(ClassDefinition sup | sup = cd.getSuperClassDefinition() and
									 hasMethod(sup, md) and
									 not exists(MethodDeclaration md2 | md2 = cd.getAMethod() and md2.getName() = md.getName())))
	}
		
	predicate isCall(MethodCallExpr mce){
		mce.getMethodName() = "call"
	} 

	predicate isApply(MethodCallExpr mce){
		mce.getMethodName() = "apply"
	} 
	
	predicate isBind(MethodCallExpr mce){
		mce.getMethodName() = "bind"
	} 

	/**
	 * if Class(C) -> Receiver(x.m(...)) and C declares method m, create edge Fun(m) -> Callee(x.m())
	 */
	predicate rMethodCall1(MethodCallExpr mce, FunNode funNode, string label, CalleeNode calleeNode){
	    (exists(MethodDeclaration methDecl, ClassDeclStmt cds, ClassDeclNode cdNode, ReceiverNode recNode | 
			not isCall(mce) and // not Function.call
	    	not (mce.getReceiver() instanceof SuperExpr) and // not a super-call
	    	calleeNode = CalleeNode(mce) and
	    	funNode = FunNode(methDecl.getBody()) and  
			hasMethod(cds, methDecl) and
	    	methDecl.getName() = mce.getCalleeName() and
	    	cdNode = ClassDeclNode(cds) and
	        flowPath(cdNode, recNode) and 
	        recNode = ReceiverNode(mce) and 
	    	label = "[MethodCall-1]"
	    )
	    or
	    exists(MethodDeclaration methDecl, ClassDeclStmt cds, ClassDeclNode cdNode, ReceiverNode recNode | 
	    	mce.getReceiver() instanceof SuperExpr and // special rule for super-calls
	    	calleeNode = CalleeNode(mce) and
	    	funNode = FunNode(methDecl.getBody()) and   
			hasMethod(cds.getSuperClassDefinition(), methDecl) and
	    	methDecl.getName() = mce.getCalleeName() and
	    	cdNode = ClassDeclNode(cds) and
	        flowPath(cdNode, recNode) and 
	        recNode = ReceiverNode(mce) and 
	    	label = "[MethodCall-super]"
		))
	}

	predicate rMethodCall2(PropAccess pa, FunNode funNode, string label, CalleeNode calleeNode){
	    exists(MethodDeclaration methDecl, ClassDeclNode cdNode, ReceiverNode recNode |  
	    	calleeNode = CalleeNode(pa) and
	    	funNode = FunNode(methDecl.getBody()) and 
	    	methDecl.getName() = pa.getPropertyName() and
	    	cdNode = ClassDeclNode(methDecl.getDeclaringClass()) and
	        flowPath(cdNode, recNode) and 
	        recNode = ReceiverNode(pa) and 
	    	label = "[MethodCall-2]"
	    )
	}

	/**
	 * for a call E = E'.call(...) such that Fun(f) -> v(E'), create edge Fun(f) -> CalleeNode(E)
	 */
	predicate rCall(MethodCallExpr mce, FunNode funNode, string label, CalleeNode target){
		isCall(mce) and 
		flowPath(funNode, v(mce.getReceiver())) and
		label = "[Call]" and
		target = CalleeNode(mce)
	}

	/**
	 * for a call E = E'.apply(...) such that Fun(f) -> v(E'), create edge Fun(f) -> CalleeNode(E)
	 */
	predicate rApply(MethodCallExpr mce, FunNode funNode, string label, CalleeNode target){
		isApply(mce) and 
		flowPath(funNode, v(mce.getReceiver())) and
		label = "[Apply]" and
		target = CalleeNode(mce)
	}

	/**
	 * for a call E = E'.bind(...) such that Fun(f) -> v(E'), create edge Fun(f) -> ResNode(E)
	 */
	predicate rBind(MethodCallExpr mce, FunNode funNode, string label, ExpNode target){
		isBind(mce) and 
		flowPath(funNode, v(mce.getReceiver())) and
		label = "[Bind]" and
		target = ExpNode(mce)
	}

    /**
	 * for a call g.m(...) where g is a global variable, and a definition of g.m occurs in an
	 * externs file, create edge Fun(g.m) -> Callee(g.m())
	 */
	predicate rExternalMethodCall(MethodCallExpr mce, FunNode funNode, string label, CalleeNode calleeNode){
		exists(GlobalVariable gv , ExternalInstanceMemberDecl md, ExternalGlobalVarDecl vd |
			not isCall(mce) and
			vd.getTypeTag().getTypeDeclaration() = md.getDeclaringType() and // `md` is declared as an instance member of the type of `vd`
        	gv.getADeclaration() = vd.getBindingPattern()  and // `vd` declares `gv`
			//mce.getReceiver() instanceof VarRef and
			gv = ((VarRef)mce.getReceiver()).getVariable() and
			calleeNode = CalleeNode(mce) and
			label = "[MethodCall-external]" and
			md.getProperty().getPropertyName() = mce.getCalleeName() and
			funNode = FunNode(md.getInit() ) 
		) 
	}

		/** 
	 * For getter access e.f^\pi where class C { get f(){ ... } } such that Class(C) ->* v(e), 
	 *    v(e) -> Receiver(\pi)
	 *    v(f) -> Callee(\pi)
	 *    Res(\pi) -> v(e.f)
	 */
	predicate rGet1(GetterMethodDefinition gmd, Node source, string label, ReceiverNode target){	
		exists(ClassDeclStmt cds, PropAccess pa, ClassDeclNode cdn | 
		    cdn = ClassDeclNode(cds) and
		    gmd.getName() = pa.getPropertyName() and 
		    gmd.getDeclaringClass() = cds and
			source = v(pa.getBase()) and
			flowPath(cdn, source) and  
			label = "[Get-1]" and 
			target = ReceiverNode(pa)
		)
	}

	predicate rGet2(GetterMethodDefinition gmd, Node source, string label, CalleeNode target){
		exists(ClassDeclStmt cds, PropAccess pa, ClassDeclNode cdn | 
		    cdn = ClassDeclNode(cds) and
		    gmd.getName() = pa.getPropertyName() and 
		    gmd.getDeclaringClass() = cds and 
			source = v(pa.getPropertyNameExpr()) and
			flowPath(cdn, v(pa.getBase())) and 
			label = "[Get-2]" and 
			target = CalleeNode(pa)
		)
	}

	predicate rGet3(GetterMethodDefinition gmd, ResNode source, string label, PropNode target){	
		exists(ClassDeclStmt cds, PropAccess pa, ClassDeclNode cdn | 
		    cdn = ClassDeclNode(cds) and
		    gmd.getName() = pa.getPropertyName() and 
		    gmd.getDeclaringClass() = cds and
		    source = ResNode(pa) and
			flowPath(cdn, v(pa.getBase())) and 
			label = "[Get-3]" and 
			target = v(pa)
		)	
	}
	
	/** 
	 * For setter access e.f^\pi where class C { set p(){ ... } } such that Class(C) ->* v(e), 
	 * create edges:
	 *    v(e) -> Receiver(\pi)
	 *    v(f) -> Callee(\pi)
	 *    Res(\pi) -> Exp(e.f)
	 */
	predicate rSet1(SetterMethodDefinition smd, Node source, string label, ReceiverNode target){
		exists(ClassDeclStmt cds, PropAccess pa, ClassDeclNode cdn |
		   smd.getDeclaringClass() = cds and
		   smd.getName() = pa.getPropertyName() and
		   cdn = ClassDeclNode(cds) and 
		   source = v(pa.getBase()) and
		   flowPath(cdn, source) and
		   label = "[Set-1]" and 
		  target = ReceiverNode(pa)
		)
	}

	predicate rSet2(SetterMethodDefinition smd, Node source, string label, CalleeNode target){	
		exists(ClassDeclStmt cds, PropAccess pa, ClassDeclNode cdn |
		   smd.getDeclaringClass() = cds and
		   smd.getName() = pa.getPropertyName() and
		   cdn = ClassDeclNode(cds) and 
		   source = v(pa.getPropertyNameExpr()) and
		   flowPath(cdn, v(pa.getBase())) and  
		   label = "[Set-2]" and 
		   target = CalleeNode(pa)
		)
	}

	predicate rSet3(SetterMethodDefinition smd, ResNode source, string label, PropNode target){
		exists(ClassDeclStmt cds, PropAccess pa, ClassDeclNode cdn |
			smd.getDeclaringClass() = cds and
			smd.getName() = pa.getPropertyName() and
			cdn = ClassDeclNode(cds) and 
		    source = ResNode(pa) and
			flowPath(cdn, v(pa.getBase() )) and 
			label = "[Set-3]" and 
			target = v(pa)
		)
	}

	predicate rSet4(SetterMethodDefinition smd, Node source, string label, ParamNode target){
		exists(PropAccess pa, FunNode funNode, CalleeNode calleeNode, Function fun, Assignment ass | 
				funNode = FunNode(fun) and 
				smd.getName() = pa.getPropertyName() and  
	    	fun.getParent() = smd and
	    	calleeNode = CalleeNode(pa) and
	        flowPath(funNode, calleeNode) and
	    	ass = pa.getParent() and
	  	    source = v(ass.getRhs()) and 
	        target = ParamNode(fun.getParameter(0)) and 
	    	label = "[Set-4]"
	    )
	}
	
	/**
	 * For a property-getter access e.f\pi, { ... get f()\pi' ... }, where Fun(\pi') -> Prop(f) create edges
	 *   Fun(\pi') -> Callee(\pi)
	 *   v(e) -> Param(\pi',0)
	 *   Res(\pi) -> Prop(f)   
	 */
	predicate rGetter1(PropertyGetter pg, FunNode source, string label, CalleeNode target){
		exists(PropAccess pa | 
			 source = FunNode(pg.getInit()) and  
			 pg.getName() = pa.getPropertyName() and  
		  //  flowPath(source, PropNode(pa.getPropertyName())) and 
		   target = CalleeNode(pa) and
		   label = "[Getter-1]"
		)
	}

	predicate rGetter2(PropertyGetter pg, Node source, string label, ThisNode target){
		exists(PropAccess pa, FunNode funNode | 
		   funNode = FunNode(pg.getInit()) and  
			//  flowPath(funNode, PropNode(pa.getPropertyName())) and
			 pg.getName() = pa.getPropertyName() and  
		   source = v(pa.getBase()) and
		   target = ThisNode(pg.getInit()) and
		   label = "[Getter-2]"
		)
	}

	predicate rGetter3(PropertyGetter pg, ResNode source, string label, PropNode target){
		exists(PropAccess pa, FunNode funNode |
		   funNode = FunNode(pg.getInit()) and  
			//  flowPath(funNode, PropNode(pa.getPropertyName())) and
			 pg.getName() = pa.getPropertyName() and  
		   source = ResNode(pa) and
		   target = PropNode(pa.getPropertyName()) and
		   label = "[Getter-3]"
		) 
	}
	
	/**
	 * For a property-setter access e.f\pi = e', { ... set f()\pi' ... }, where Fun(\pi') -> Prop(f) create edges
	 *   Fun(\pi') -> Callee(\pi)
	 *   v(e) -> Param(\pi',0)
	 *   Res(\pi) -> Prop(f)
	 *   v(e') -> Param(\pi',1)   
	 */
	predicate rSetter1(PropertySetter ps, FunNode source, string label, CalleeNode target){
		exists(PropAccess pa |
			source = FunNode(ps.getInit()) and  
			ps.getName() = pa.getPropertyName() and  
		  // flowPath(source, PropNode(pa.getPropertyName())) and 
		  target = CalleeNode(pa) and
		  label = "[Setter-1]"
		)
	}

	predicate rSetter2(PropertySetter ps, Node source, string label, ThisNode target){
		exists(FunNode funNode, PropAccess pa |
		  funNode = FunNode(ps.getInit()) and  
			// flowPath(funNode, PropNode(pa.getPropertyName())) and
			ps.getName() = pa.getPropertyName() and  
		  source = v(pa.getBase()) and
		  target = ThisNode(ps.getInit()) and
		  label = "[Setter-2]"  
		)
	}
		  
	predicate rSetter3(PropertySetter ps, ResNode source, string label, PropNode target){
		exists(FunNode funNode, PropAccess pa |
		  funNode = FunNode(ps.getInit()) and  
			// flowPath(funNode, PropNode(pa.getPropertyName())) and
			ps.getName() = pa.getPropertyName() and  
		  source = ResNode(pa) and
		  target = PropNode(pa.getPropertyName()) and
		  label = "[Setter-3]"
		)
	}
		  
	predicate rSetter4(PropertySetter ps, Node source, string label, ParamNode target){
		exists(Assignment ass, FunNode funNode, PropAccess pa |
		  funNode = FunNode(ps.getInit()) and   
			// flowPath(funNode, PropNode(pa.getPropertyName())) and
			ps.getName() = pa.getPropertyName() and  
		  pa.getParent() = ass and
		  source = v(ass.getRhs()) and
		  target = ParamNode(ps.getInit().getParameter(0)) and
		  label = "[Setter-4]"
		 )
	}
	
	/**
	 *  for "require(n)", create edge ExportMod(n) -> ImportMod(\pi, n)  
	 */
	

	predicate rRequire1(InvokeExpr ie, CommonJSExportModNode source, string label, CommonJSImportModNode target){ 
		exists(PropAccess pa, File file | 
			isModuleExport(pa) and 
			file = pa.getFile() and
			file = getRequiredFile(ie) and 
			isCallToRequire(ie) and  
			source = CommonJSExportModNode(pa.getTopLevel()) and
			target = CommonJSImportModNode(ie) and
			label = "[Require]"      
		)
	}

	predicate rRequire2(InvokeExpr ie, ESMExportModNode source, string label, CommonJSImportModNode target){ 
		exists(TopLevel top, File file |  
			file = top.getFile() and
			file = getRequiredFile(ie) and 
			isCallToRequire(ie) and  
			source = ESMExportModNode(top) and
			target = CommonJSImportModNode(ie) and
			label = "[Require]"      
		)
	}
	
	/**
	 *  for "import s from "n", create edges ExportMod(n) -> ImportMod(\pi, n) and ImportMod(\pi, n) -> Var(s)
	 */
	predicate rImport1(ImportDeclaration id, ESMExportModNode source, string label, ESMImportModNode target){
		exists(TopLevel top |
			top.getFile() = getImportedFile(id) and
			source = ESMExportModNode(top) and 
			target = ESMImportModNode(id) and 
			label = "[Import]"      
		)
	}
	
	predicate rImport2(ImportDeclaration id, CommonJSExportModNode source, string label, ESMImportModNode target){
		exists(TopLevel top |
			top.getFile() = getImportedFile(id) and
			source = CommonJSExportModNode(top) and 
			target = ESMImportModNode(id) and 
			label = "[Import]"      
		)
	}
	
	/**
	 *  given ExportMod(n) -> ImportMod(n), create edges ExportName(n,p) -> ImportName(n,p)
	 */
	predicate rImportNames1(InvokeExpr ie, CommonJSExportNameNode source, string label, CommonJSImportNameNode target){
		exists(CommonJSExportModNode expNode, CommonJSImportModNode impNode, TopLevel top, PropAccess pa, string name  |  
			expNode = CommonJSExportModNode(top) and 
			impNode = CommonJSImportModNode(ie) and 
			flowEdge(_, expNode, _, impNode) and 
			isPropertyExport(pa) and name = pa.getPropertyName() and pa.getTopLevel() = top and // find property export in same file  	
			source = CommonJSExportNameNode(pa) and  
		  target = CommonJSImportNameNode(ie, name) and 
		  label = "[ImportNames]" 
		)       
	}

	predicate rImportNames2(ImportDeclaration id, ESMExportNameNode source, string label, ESMImportNameNode target){
		exists(ExportDeclaration ed, ESMExportModNode meNode, ESMImportModNode impNode, string name, File file  |  
			 impNode = ESMImportModNode(id) and 
			 ed.getFile() = file and
		     meNode = ESMExportModNode(ed.getTopLevel()) and 
		     flowEdge(_, meNode, _, impNode) and 
		     source = ESMExportNameNode(file, name) and 
		     target = ESMImportNameNode(id, name) and 
		     label = "[ImportNames]" 
		)       
	}

	predicate rImportNames3(ImportDeclaration id, CommonJSExportNameNode source, string label, ESMImportNameNode target){
		exists(CommonJSExportModNode expNode, ESMImportModNode impNode, TopLevel top, PropAccess pa,  string name |  
			expNode = CommonJSExportModNode(pa.getTopLevel()) and 
			impNode = ESMImportModNode(id) and 
			flowEdge(_, expNode, _, impNode) and 
      isPropertyExport(pa) and name = pa.getPropertyName() and pa.getTopLevel() = top and // find property export in same file
			source = CommonJSExportNameNode(pa) and  
		  target = ESMImportNameNode(id, name) and 
		  label = "[ImportNames]" 
		)       
	}
	 
	/**
	 *  For named imports "import { v_1, v_2, .. } from "n", create edges ImportName{\pi}(v_i) -> Var(v_i)  
	 */
	predicate rNamedImport(ImportSpecifier is, ESMImportNameNode impNode, string label, VarNode target){    
	    exists(ImportDeclaration id | 
	        is = id.getASpecifier() and   
	        impNode = ESMImportNameNode(id, is.getImported().getName()) and
			target = VarNode(is.getLocal()) and 
	        label = "[NamedImport]"   
	    )
	}
	
	/**
	 * if there is an edge ExportMod(ed) -> ImportMod(r) such that ed is a default export and
	 * r is a call to require, then create an edge ExportMod(ed) -> ImportName(r, "default")
	 */
	predicate rAccessDefaultImportViaRequire(ExportDefaultDeclaration ed, ESMExportModNode source, string label, CommonJSImportNameNode target){ 
		source = ESMExportModNode(ed.getTopLevel()) and
		exists(InvokeExpr ie, CommonJSImportModNode im | 
			ed.getFile() = getRequiredFile(ie) and
			isCallToRequire(ie) and 
			im = CommonJSImportModNode(ie) and
			flowEdge(_, source, _, im) and
	    	target = CommonJSImportNameNode(ie, "default") and
			label = "[AccessDefaultViaRequire]"  
		)       
	}
	
   /**
	 * For "import v from n" at \pi, generate ImportMod{\pi}{n} -> Var(v)
	 */
	predicate rImportDefault(ImportDeclaration id, ESMImportModNode source, string label, VarNode target){
		exists(ImportDefaultSpecifier is | 
		        source = ESMImportModNode(id) and 
		        is = id.getASpecifier() and  
		        target =  VarNode(is.getLocal()) and
		        label = "[ImportDefault]"       
		)
	}
	
  predicate rNamedExport(ExportNamedDeclaration ed, Node source, string label, ESMExportNameNode target){	
		 exists(Variable var, string name |
			  source = VarNode(var.getADeclaration()) and
				ed.exportsAs(var, name) and //name = ed.getASpecifier().getExportedName() and
				target = ESMExportNameNode(ed.getFile(),name) and
				label = "[NamedExport]" 
			)
	}

	/**
	 *  For "export default e", create edges v(e) -> ExportMod(n) 
	 */
	predicate rDefaultExport1(ExportDefaultDeclaration ed, Node source, string label, ESMExportModNode target){ 
		source = v(ed.getOperand()) and 
		target = ESMExportModNode(ed.getTopLevel()) and 
		label = "[DefaultExport-Exp]"
	}

	predicate rDefaultExport2(ExportDefaultDeclaration ed, VarNode source, string label, ESMExportModNode target){ 
		source = VarNode(((FunctionDeclStmt)ed.getOperand()).getId()) and  
		target = ESMExportModNode(ed.getTopLevel()) and
		label = "[DefaultExport-Fun]"
	}

	predicate rDefaultExport3(ExportDefaultDeclaration ed, VarNode source, string label, ESMExportModNode target){ 
		source = VarNode(((ClassDeclStmt)ed.getOperand()).getIdentifier()) and  
		target = ESMExportModNode(ed.getTopLevel()) and
		label = "[DefaultExport-Class]"
	}
	
	/**
	 * For "export { v as default }" in file n generate edge Var(v) -> ExportMod(n)
	 */
	predicate rDefaultExportSpecifier(ExportNamedDeclaration ed, VarNode source, string label, ESMExportModNode target){
		exists(ExportSpecifier es, VarRef vr |  
		    es = ed.getASpecifier() and 
		    es.getExportedName() = "default" and 
		    vr.getParent() = es and  
		    source = VarNode(vr.getAVariable().getADeclaration())  and 
		    target = ESMExportModNode(ed.getTopLevel()) and 
		    label = "[DefaultExport-Specifier]"
		)
	}
	
	/**
	 *  For re-export "export ... from n", create edges ExportMod(n) -> ExportMod(n') 
	 */
	predicate rReExportMod(ReExportDeclaration re, ESMExportModNode source, string label, ESMExportModNode target){ 
		exists(ExportDeclaration ed, File file |
			file = ed.getFile() and
			// reachableASTNode(re) and    
			file = getReExportedFile(re) and
			source = ESMExportModNode(ed.getTopLevel()) and
			target = ESMExportModNode(re.getTopLevel()) and
			label = "[ReExport-Mod]"
		)
	}
	
	/**
	 *  For bulk re-export "export * from n", create edges ExportName(n, f) -> ExportName(n', f) for each f
	 */
	predicate rBulkReExport(BulkReExportDeclaration be, ESMExportNameNode source, string label, ESMExportNameNode target){ 
		exists(string name |   
		    source = ESMExportNameNode(getReExportedFile(be), name) and
		    target = ESMExportNameNode(be.getFile(), name) and
		    label = "[BulkReExport]"
		)
	}
	
	/**
	 *  For selective re-export "export f as f' from n", create edges ExportName(n, f) -> ExportName(n', f') 
	 */
	predicate rSelectiveReExport(SelectiveReExportDeclaration se, ESMExportNameNode source, string label, ESMExportNameNode target){ 
		exists(ExportSpecifier es|  
		    es = se.getASpecifier() and 
		    es.getExportedName() != "default" and
		    source = ESMExportNameNode(getReExportedFile(se), es.getLocalName()) and
		    target = ESMExportNameNode(se.getFile(), es.getExportedName()) and
		 	label = "[SelectiveReExport]"
		)
	}
	
	/**
	 *  For selective re-export "export { default as f } from n", create edges ExportMod(n) -> ExportName(n', f) 
	 */
	predicate rReExportDefault(SelectiveReExportDeclaration se, ESMExportModNode source, string label, ESMExportNameNode target){ 
		exists(ExportDeclaration ed, File file, ExportSpecifier es| 
			file = ed.getFile() and
			file = getReExportedFile(se) and
		    es = se.getASpecifier() and 
		    es.getLocalName() = "default" and 
		    source = ESMExportModNode(ed.getTopLevel()) and
		    target =  ESMExportNameNode(se.getFile(), es.getExportedName()) and
		 	label = "[ReExportDefault]"
		)
	}
	
	predicate flowPath(Node source, Node target){
		source = target 
		or
		exists(Node t1 | flowEdge(_, source, _, t1) and flowPath(t1, target))
	}
	
	predicate flowEdge(ASTNode a, Node source, string label, Node target){
		rParenthesizedExpr(a, source, label, target) or
		rAssign1(a, source, label, target) or
		rAssign2(a, source, label, target) or
		rAssign3(a, source, label, target) or
		rAssign4(a, source, label, target) or
		rAssign5(a, source, label, target) or
		rAssign6(a, source, label, target) or
		rDestructuringAssign1(a, source, label, target) or
		rDestructuringAssign2(a, source, label, target) or
		rLogicalOr(a, source, label, target) or
		rConditionalExpr(a, source, label, target) or
		rLogicalAnd(a, source, label, target) or
		rObjLiteral(a, source, label, target) or
		rFunctionExpr1(a, source, label, target) or
		rFunctionExpr2(a, source, label, target) or
		rArrowFunctionExpr(a, source, label, target) or
		rFunctionDecl(a, source, label, target) or
		rFunctionCall1(a, source, label, target) or
		rFunctionCall2(a, source, label, target) or
		rFunctionCall3(a, source, label, target) or
		rFunctionCallReceiver(a, source, label, target) or
		rSuperCall(a, source, label, target) or
		rReturn(a, source, label, target) or
		rArgToParam1(a, source, label, target) or
		rArgToParam2(a, source, label, target) or  
		rCallArgToParam1(a, source, label, target) or
		rCallArgToParam2(a, source, label, target) or
		rApplyArgToParam(a, source, label, target) or
		rBindArgToParam(a, source, label, target) or  
		rRetToRes(a, source, label, target) or
		rClassDecl(a, source, label, target) or
		rMethodDecl(a, source, label, target) or
		rCtorCall(a, source, label, target) or 
		rMethodCall1(a, source, label, target) or 
		rMethodCall2(a, source, label, target) or  
		rCall(a, source, label, target) or  
		rApply(a, source, label, target) or  
		rBind(a, source, label, target) or  
		rExternalMethodCall(a, source, label, target) or 
		rGet1(a, source, label, target) or
		rGet2(a, source, label, target) or
		rGet3(a, source, label, target) or
		rSet1(a, source, label, target) or
		rSet2(a, source, label, target) or
		rSet3(a, source, label, target) or
		rSet4(a, source, label, target) or

		rGetter1(a, source, label, target) or
		rGetter2(a, source, label, target) or
		rGetter3(a, source, label, target) or

		rSetter1(a, source, label, target) or
		rSetter2(a, source, label, target) or
		rSetter3(a, source, label, target) or
		rSetter4(a, source, label, target) or
		
		rRequire1(a, source, label, target) or
		rRequire2(a, source, label, target) or

		rImport1(a, source, label, target) or
		rImport2(a, source, label, target) or

		rImportNames1(a, source, label, target) or
		rImportNames2(a, source, label, target) or
		rImportNames3(a, source, label, target) or

		rImportDefault(a, source, label, target) or
		rNamedImport(a, source, label, target) or  
		rAccessDefaultImportViaRequire(a, source, label, target) or 
		
		rNamedExport(a, source, label, target) or 
		rDefaultExport1(a, source, label, target) or
		rDefaultExport2(a, source, label, target) or
		rDefaultExport3(a, source, label, target) or 
		rDefaultExportSpecifier(a, source, label, target) or
		rReExportMod(a, source, label, target) or
		rBulkReExport(a, source, label, target) or 
		rSelectiveReExport(a, source, label, target) or
		rReExportDefault(a, source, label, target)
	}
	
	/**
	 * Compute a call graph edge from a CalleeNode to a FunctionNode if there exists a path from
	 * the latter to the former in the flow graph
	 */
	predicate callGraphEdge(CalleeNode calleeNode, FunNode funNode){
		exists(Expr callsite, Function target |
			(not (isNativeFile(callsite.getFile()) and isNativeFile(target.getFile())) ) and
			calleeNode = CalleeNode(callsite) and 
			funNode = FunNode(target) and
			flowPath(funNode, calleeNode)
		)
	}
	
	// ----------------------------------------------------------------------------------- //
	
	/**
	 * A file is reachable from the mainFile if it is the main file itself, or 
	 * if it's reachable via import/require/re-export
	 */
	predicate reachable(File file){
		not isExcluded(file)
		and
		(isMainFile(file)
		or
		(isNativeFile(file))
		or 
		exists(File f |
			reachable(f) and
			// a file is reachable if it is required by a reachable file
			(exists(InvokeExpr ie |  f = ie.getFile() and file = getRequiredFile(ie)) or
			// a file is reachable if it is imported by a reachable file
			exists(ImportDeclaration id | f = id.getFile() and file = getImportedFile(id)) or
			// a file is reachable if a reachable file re-exports from it
			exists(ReExportDeclaration ed | reachable(f) and f = ed.getFile() and file = getReExportedFile(ed))
		   )))
	}	
	 
	/** 
	 * An ASTNode is reachable if it occurs at the top level in a reachable file,
	 * or if it occurs in a function that is part of the call graph
	 */
	predicate reachableASTNode(ASTNode a){
		reachable(a.getFile())
		and
		(a.getContainer() instanceof TopLevel)
		or
		exists(FunNode funNode | funNode = FunNode(a.getContainer()) and callGraphEdge(_, funNode)
		)
	}

	/** override this predicate in a subclass to specify the main file
	 *  
	 */
	abstract predicate isMainFile(File file);
	
	/** override this predicate in a subclass to specify files that should be ignored
	 *  (by default, no files are ignored
	 */
	predicate isExcluded(File file){
		none()
	}
	
	// a file is reachable if it is the node native library
	predicate isNativeFile(File file) {
		file.getAbsolutePath().indexOf("externs/es") != -1 or
		file.getAbsolutePath().indexOf("externs/lib/bdd.js") != -1 or
		file.getAbsolutePath().indexOf("externs/lib/jest.js") != -1 or
		file.getAbsolutePath().indexOf("externs/nodejs/globals.js") != -1
	}
		   
}
