local sd = require("symdiff")
local x = sd.var("x")
local y = sd.var("y")
local poly = 4*x^2 + 3*y^3 - 2*x*y
local temp = poly:evaluate{[x] = 2}  -- substitutes 2 in place of x, we get a new expression
print(temp)  -- prints "16 + 3*y^3 - 4*y"
print(temp:evaluate(-1))  -- now that we're back to a single variable,

print(poly:derivative(x))
print(poly:derivative(y))  -- derivative with respect to y, prints
local ddx = poly:derivative(x)  -- previous result was cached, no worries about performance
local ddx_ddy = ddx:derivative(y)
print(ddx_ddy)