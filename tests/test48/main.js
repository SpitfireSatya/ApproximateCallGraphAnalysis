function Product(name, price, fun) {
this.name = name;
this.price = price;
this.fun = fun;
}
 
function Food(name, price, fun) {
Product.call(this, name, price, fun);
this.category = 'food';
}
 
function Toy(name, price, fun) {
Product.call(this, name, price, fun);
this.category = 'toy';
}
 
var cheese = new Food('feta', 5, function(){ console.log("cheese"); });
var robot = new Toy('robot', 40, function(){ console.log("robot"); }); 
cheese.fun();
robot.fun();