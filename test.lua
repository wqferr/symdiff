local sd = require("symdiff")
local x = sd.var("x")
local y = sd.var("y")
local expr = y*sd.ln(4*x^2)
print(expr)
print(expr:derivative(x))