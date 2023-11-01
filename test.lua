local sd = require "symdiff"

local z = sd.var "z"
-- local expr = sd.ln(symdiff.sqrt(z^3))
local expr = sd.sqrt(z)
print(expr.derivative(0))