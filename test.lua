local symdiff = require "symdiff"

local function printDeps(expr)
    print "start"
    for var, _ in pairs(expr.dependencies) do
        print(var.name)
    end
    print "-----"
end

local z = symdiff.var "z"
-- local t = symdiff.var "t"
local expr = z^3
print("HERE")
-- local d1 = expr.derivative[z]
local d1 = expr:calculateDerivative(z)
printDeps(d1)
local d2 = d1.derivative[z]
print(d2)
-- local expr = z^z - t*z + 1
-- print(expr.derivative[z])
-- print((symdiff.sin(1).derivative[z]))
-- print(expr:evaluate {[z] = 2, [t] = -1})
-- print(expr:evaluate {[z] = 2})
-- print(expr.derivative[t]:evaluate {[z] = 0})
-- print(expr.derivative[z])

-- printDeps(2*z)
-- printDeps(expr)
-- printDeps(expr.derivative[t])
-- printDeps(expr.derivative[z])
-- printDeps(expr.derivative[z].derivative[z])