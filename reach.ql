import javascript

/**
 * Find the file referenced in an "import" statement. We do this by first computing the absolute path of the
 * referenced file, and then finding a File object corresponding to that path. 
 */
File getImportedFile(ImportDeclaration id){ 
	exists(File file, string importPath, string prefix |
	   importPath = id.getImportedPath().getValue() and
	   prefix = id.getFile().getAbsolutePath().substring(0, id.getFile().getAbsolutePath().indexOf(id.getFile().getBaseName())) and
	   ( file.getAbsolutePath() = importPath.replaceAll("./", prefix) or
	     file.getAbsolutePath() = importPath.replaceAll("./", prefix) + ".js") and 
	   result = file
	)
}

File getReExportedFile(ReExportDeclaration be){
	exists(File file, string importPath, string prefix |
	   importPath = be.getImportedPath().toString() and
	   prefix = be.getFile().getAbsolutePath().substring(0, be.getFile().getAbsolutePath().indexOf(be.getFile().getBaseName())) and
	   (file.getAbsolutePath() = importPath.replaceAll("./", prefix).substring(1,importPath.replaceAll("./", prefix).length()-1) or 
	    file.getAbsolutePath() = importPath.replaceAll("./", prefix).substring(1,importPath.replaceAll("./", prefix).length()-1) + ".js"
	   )
	   and 
	   result = file
	)
}

/**
 * Find the file referenced in a "require" expression. We do this by first computing the absolute path of the
 * referenced file, and then finding a File object corresponding to that path. 
 */
File getRequiredFile(InvokeExpr ie){
	// require a .js file
	ie.getCalleeName() = "require" and
	(exists(File file, string importPath, string prefix |
	   importPath = ((StringLiteral)ie.getChild(0)).getStringValue() and
	   prefix = ie.getFile().getAbsolutePath().substring(0, ie.getFile().getAbsolutePath().indexOf(ie.getFile().getBaseName())) and
	   file.getAbsolutePath() = importPath.replaceAll("./", prefix) and 
	   result = file
	)
	or
	// require a directory relative to "."; in this case we look for an index.js file within that directory
	exists(File file, string importPath, string prefix |
	   importPath = ((StringLiteral)ie.getChild(0)).getStringValue() and
	   prefix = ie.getFile().getAbsolutePath().substring(0, ie.getFile().getAbsolutePath().indexOf(ie.getFile().getBaseName())) and
	   file.getAbsolutePath().indexOf(importPath.replaceAll("./", prefix)) = 0 and 
	   file.getBaseName() = "index.js" and
	   result = file
	)
	or
	// require a directory relative to ".."; in this case we look for an index.js file within that directory
	exists(File file, string importPath, string prefix, Container parent |
	   parent = ie.getFile().getParentContainer().getParentContainer() and
	   importPath = ((StringLiteral)ie.getChild(0)).getStringValue() and
	   prefix = parent.getAbsolutePath() and
	   file.getAbsolutePath().indexOf(importPath.replaceAll("../", prefix + "/")) = 0 and 
	   file.getBaseName() = "index.js" and
	   result = file
	))
}

predicate isMainFile(File file){
        file.getBaseName() = "MemoryFileSystem.js" 
        or
        file.getBaseName() = "MemoryFileSystemError.js" 
}
    
/*     
boolean isExcluded(File file){
	if (file.getAbsolutePath().indexOf("node_modules") != -1) then
		result = true
	else
		result = false
}*/

predicate findInvokeExpr(File file, InvokeExpr ie){
   ie.getFile() = file 	 
}

predicate findImportDeclaration(File file, ImportDeclaration id){
   id.getFile() = file 	 	
}

predicate findReExportDeclaration(File file, ReExportDeclaration rd){
   rd.getFile() = file 	
}

predicate reachable(File file){
	//isExcluded(file) = false
	//and
	(isMainFile(file)
	or 
	// a file is reachable if it is required by a reachable file
	exists(File f, InvokeExpr ie | reachable(f) and findInvokeExpr(f, ie) and file  = getRequiredFile(ie) )
	or
	// a file is reachable if it is imported by a reachable file
	exists(File f, ImportDeclaration id | reachable(f) and findImportDeclaration(f, id) and file = getImportedFile(id) )
    or
    // a file is reachable if a reachable file re-exports from it
    exists(File f, ReExportDeclaration rd | reachable(f) and findReExportDeclaration(f, rd) and file = getReExportedFile(rd) )
    )
}

//from File s 
//    where reachable(s)
//select s.getAbsolutePath()


from File s, ASTNode a
where 
  a.getFile() = s and
  a instanceof InvokeExpr or a instanceof ImportDeclaration or a instanceof ReExportDeclaration
select
  s, s.getAbsolutePath()

