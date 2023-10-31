local symdiff = require "symdiff"

local z = symdiff.var "z"
local expr = symdiff.ln(symdiff.sqrt(z))
-- local expr = symdiff.ln(z)
-- local expr = symdiff.sqrt(z)

-- print(z:calculateDerivative(z))
-- print(z.derivative[z])
print(expr.derivative[z])