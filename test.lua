local symdiff = require "symdiff"

local z = symdiff.var "z"
local t = symdiff.var "t"
-- local expr = z*z + z
-- local expr = z*z - z
local expr = z^3 - z

print(expr.derivative[z])