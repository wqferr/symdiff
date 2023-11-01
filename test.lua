local sd = require "symdiff"

local z = sd.var "z"
local expr = sd.ln(sd.sqrt(z^3))
print(expr.derivative(3))
print(expr.derivative[z]:evaluate{[z] = 3})