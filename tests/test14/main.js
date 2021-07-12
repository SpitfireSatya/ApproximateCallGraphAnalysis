var TokenType = function TokenType(label, conf) {
   if ( conf === void 0 ) conf = {};

   this.label = label;
   this.keyword = conf.keyword;
   this.beforeExpr = !!conf.beforeExpr;
   this.startsExpr = !!conf.startsExpr;
   this.isLoop = !!conf.isLoop;
   this.isAssign = !!conf.isAssign;
   this.prefix = !!conf.prefix;
   this.postfix = !!conf.postfix;
   this.binop = conf.binop || null;
   this.updateContext = null;
};

var beforeExpr = {beforeExpr: true}, startsExpr = {startsExpr: true};

var types = {
  num: new TokenType("num", startsExpr),
  regexp: new TokenType("regexp", startsExpr),
  string: new TokenType("string", startsExpr),
  name: new TokenType("name", startsExpr),
  eof: new TokenType("eof")
}

console.log("done");
