local sd = require "symdiff"

local z = sd.var "z"
local expr = sd.ln(z + z^2)
print(expr:derivative())
print(expr:derivative(z))
print(expr:derivative(3))
print(expr:derivative(z):evaluate(3))
print(expr:derivative(z):evaluate{[z] = 3})