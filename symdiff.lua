--[[
    TODO
    merge consecutive sum / product nodes
    add proper subtraction and unm nodes
    fix dependency tracking when taking derivatives
]]
--[[
Copyright © 2023 William Quelho Ferreira

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is furnished
to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
]]

---@diagnostic disable-next-line: deprecated
local unpack = unpack or table.unpack

local M = {}

--@field derivative Expression|fun(self: table, point: number): number
--@field cachedDerivatives {Variable: Expression}
--@field isConstant boolean?

---@class Variable: Expression

---@class Expression
---@field name string?
---@field nodeType string
---@field eval fun(self: Expression, point: {Variable: number}): number|Expression
---@field format fun(self): string
---@field dependencies {Variable: boolean}
---@field calculateDerivative fun(self: Expression, variable: Variable)
---@field parents Expression[]
---@field derivative DerivativeAccessor
---@field func fun(arg: number): number
---@field funcDerivative fun(arg: number): number
---@field repr string?
---@field funcWrapper table?
M.Expression = {}
local Expression__meta = {}
Expression__meta.__index = M.Expression

---@class Function
---@field name string
---@field func fun(arg: number): number
---@field funcDerivative fun(arg: number): number
---@field actsOnExpressions boolean
---@field setDerivative fun(self: Function, arg: Function)


---@class DerivativeAccessor: {Variable: Expression}
local DerivativeAccessor__meta = {}
DerivativeAccessor__meta.__index = function(t, k)
    if not rawget(t, k) then
        t[k] = t.expression:calculateDerivative(k)
    end
    return t[k]
end
-- TODO __call when dependencies has only one element

local nodeTypes = {
    const = "constant",
    var = "variable",
    sum = "sum",
    mul = "product",
    div = "division",
    pow = "power",
    func = "function"
}
setmetatable(nodeTypes, {__index = function(t, k) error("Unknown nodeType: "..tostring(k)) end})

local priorities = {
    [nodeTypes.const] = 10,
    [nodeTypes.var] = 10,
    -- [nodeTypes.unm] = 9
    [nodeTypes.pow] = 8,
    [nodeTypes.mul] = 2,
    [nodeTypes.div] = 2,
    -- [nodeTypes.sub] = 2,
    [nodeTypes.sum] = 1,
}
local function priority(expression)
    return priorities[expression.nodeType]
end

local function createDerivativeAccessor(expression)
    local accessor = setmetatable({}, DerivativeAccessor__meta)
    accessor.expression = expression
    return accessor
end

local function clone(t)
    local new = {}
    for k, v in pairs(t) do
        new[k] = v
    end
    return new
end

---@param dependent Expression
---@param dependencyVariable Variable
local function addDependency(dependent, dependencyVariable)
    dependent.dependencies[dependencyVariable] = true
end

local function merge(onto, new)
    for k, v in pairs(new) do
        onto[k] = v
    end
end

-- Used for evaluating constants
local nullPoint = {}
---@nodiscard
local function createBaseExpression(nodeType, eval, derivative, format, parents)
    local expr = setmetatable({}, Expression__meta)
    expr.nodeType = nodeType
    expr.eval = eval
    expr.calculateDerivative = derivative
    expr.format = format
    expr.derivative = createDerivativeAccessor(expr)
    expr.dependencies = {}
    for _, parent in ipairs(parents or {}) do
        merge(expr.dependencies, parent.dependencies)
    end
    expr.parents = parents
    return expr
end

---@param point {Variable: number}
function M.Expression:evaluate(point)
    local result = self:eval(point)
    if type(result) ~= "number" then
        -- for var, _ in pairs(point) do
        --     result.dependencies[var] = nil
        -- end
    end
    return result
end

local function varEval(self, point)
    local givenValue = point[self]
    if givenValue then
        return givenValue
    else
        return self
    end
end
local zero
local function varDerivative(self, withRespectTo)
    if self == withRespectTo then
        return M.const(1)
    else
        return zero
    end
end
local function varFormat(self)
    return self.name
end

---@return Variable
function M.var(name)
    ---@type Variable
    ---@diagnostic disable-next-line: assign-type-mismatch
    local v = createBaseExpression(nodeTypes.var, varEval, varDerivative, varFormat)
    addDependency(v, v)
    v.name = name

    return v
end

local function createConstEval(value)
    return function(_, _)
        return value
    end
end

local function constDerivative(_self, _withRespectTo)
    return zero
end

local function constFormat(self)
    if self.name then
        return self.name
    else
        local value = self:evaluate()
        if value % 1 == 0 then
            return ("%d"):format(value)
        else
            return ("%.3f"):format(value)
        end
    end
end

function M.const(value, name)
    local c = createBaseExpression(nodeTypes.const, createConstEval(value), constDerivative, constFormat)
    c.name = name
    return c
end
zero = M.const(0)
M.zero = zero

local function isConstant(expr)
    return expr.nodeType == nodeTypes.const
end

local function sumEval(self, point)
    return self.parents[1]:evaluate(point) + self.parents[2]:evaluate(point)
end
local function sumDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    return self.parents[1].derivative[withRespectTo] +
        self.parents[2].derivative[withRespectTo]
end
local function sumFormat(self)
    return ("%s + %s"):format(tostring(self.parents[1]), tostring(self.parents[2]))
end
---@param a Expression|number
---@param b Expression|number
---@return Expression
Expression__meta.__add = function(a, b)
    if type(a) == "number" then
        a = M.const(a)
    end
    if type(b) == "number" then
        b = M.const(b)
    end
    if isConstant(a) then
        if a:evaluate(nullPoint) == 0 then
            return b
        elseif isConstant(b) then
            return M.const(a:evaluate(nullPoint) + b:evaluate(nullPoint))
        end
    elseif isConstant(b) and b:evaluate(nullPoint) == 0 then
        return a
    end
    local sum = createBaseExpression(nodeTypes.sum, sumEval, sumDerivative, sumFormat, {a, b})
    return sum
end
Expression__meta.__sub = function(a, b)
    return a + -b
end

Expression__meta.__unm = function(a)
    return -1 * a
end

local function wrapParentsIfNeeded(expr, parents)
    local results = {}
    local ps = priority(expr)
    for _, p in ipairs(parents) do
        local s = tostring(p)
        if priority(p) < ps then
            s = "("..s..")"
        end
        table.insert(results, s)
    end
    return unpack(results)
end

local function productEval(self, point)
    return self.parents[1]:evaluate(point) * self.parents[2]:evaluate(point)
end
local function constProductDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    return self.parents[1]:evaluate(0) * --> constant!
        self.parents[2].derivative[withRespectTo]
end
local function productDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    local p1, p2 = self.parents[1], self.parents[2]
    return p1*p2.derivative[withRespectTo] + p1.derivative[withRespectTo]*p2
end
local function productFormat(self)
    local p1, p2 = wrapParentsIfNeeded(self, self.parents)
    return ("%s * %s"):format(p1, p2)
end
local function constProduct(const, expr)
    assert(isConstant(const), "First argument to constProduct must be a Constant")
    local product = createBaseExpression(nodeTypes.mul, productEval, constProductDerivative, productFormat, {const, expr})
    return product
end

Expression__meta.__mul = function(a, b)
    if type(b) == "number" then
        a, b = b, a
    end
    if type(a) == "number" then
        a = M.const(a)
    end
    if isConstant(a) then
        if a:evaluate(nullPoint) == 0 then
            return zero
        elseif isConstant(b) then
            if b:evaluate(nullPoint) == 0 then
                return zero
            else
                return M.const(a:evaluate(nullPoint) * b:evaluate(nullPoint))
            end
        elseif a:evaluate(nullPoint) == 1 then
            return b
        else
            return constProduct(a, b)
        end
    end
    local product = createBaseExpression(nodeTypes.mul, productEval, productDerivative, productFormat, {a, b})
    return product
end

local function quotientEval(self, point)
    return self.parents[1](point) / self.parents[2](point)
end
local function quotientDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    local a, b = self.parents[1], self.parents[2]
    return (a.derivative[withRespectTo] * b - a * b.derivative[withRespectTo]) / (b * b)
end
local function quotientFormat(self)
    local p1, p2 = wrapParentsIfNeeded(self, self.parents)
    return ("%s / %s"):format(p1, p2)
end
Expression__meta.__div = function(a, b)
    if type(a) == "number" then
        a = M.const(a)
    end
    if type(b) == "number" then
        return (1/b) * a
    elseif isConstant(b) then
        if isConstant(a) then
            return M.const(a:evaluate(nullPoint)/b:evaluate(nullPoint))
        else
            return (1/b:evaluate(nullPoint)) * a
        end
    end
    local quotient = createBaseExpression(nodeTypes.div, quotientEval, quotientDerivative, quotientFormat, {a, b})
    return quotient
end

local function powerEval(self, point)
    return self.parents[1]:evaluate(point) ^ self.parents[2]:evaluate(point)
end
local function powerRuleDerivative(self, withRespectTo)
    assert(isConstant(self.parents[2]), "Power rule only works for constant exponents")
    if not self.dependencies[withRespectTo] then
        return zero
    end
    local result = self.parents[2] * self.parents[1] ^ (self.parents[2] - 1)
    if type(result) ~= "number" then
        result.dependencies[withRespectTo] = nil
    end
    return result
end
local function constantBasePowerDerivative(self, withRespectTo)
    assert(isConstant(self.parents[1]), "Constant base power derivative requires constant base")
    if not self.dependencies[withRespectTo] then
        return zero
    end
    local result = math.log(self.parents[1]:evaluate(0)) *
        (self.parents[1]^self.parents[2]) *
        self.parents[2].derivative[withRespectTo]
    if type(result) ~= "number" then
        result.dependencies[withRespectTo] = nil
    end
    return result
end
local function generalPowerDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    -- d/dx (f(x)^g(x)) =
    -- f(x)^g(x) * (g'(x)ln(f(x)) + (f'(x)g(x))/f(x))
    local result = (self.parents[1] ^ self.parents[2]) *
        (
            self.parents[2].derivative[withRespectTo]*M.ln(self.parents[1]) +
            (self.parents[1].derivative[withRespectTo]*self.parents[2]) / self.parents[1]
        )
    if type(result) ~= "number" then
        result.dependencies[withRespectTo] = nil
    end
    return result
end
local function powerFormat(self)
    local p1, p2 = wrapParentsIfNeeded(self, self.parents)
    return ("%s^%s"):format(p1, p2)
end
Expression__meta.__pow = function(a, b)
    if type(a) == "number" then
        a = M.const(a)
    end
    if type(b) == "number" then
        b = M.const(b)
    end
    local calculateDerivative
    if isConstant(b) then
        if b:evaluate(nullPoint) == 0 then
            return M.const(1)
        elseif b:evaluate(nullPoint) == 1 then
            return a
        end
        calculateDerivative = powerRuleDerivative
    elseif isConstant(a) then
        if a:evaluate(nullPoint) == 0 then
            return zero
        end
        calculateDerivative = constantBasePowerDerivative
    else
        calculateDerivative = generalPowerDerivative
    end
    local power = createBaseExpression(nodeTypes.pow, powerEval, calculateDerivative, powerFormat, {a, b})
    return power
end

Expression__meta.__tostring = function(self)
    if type(self.format) == "string" then
        return self.format
    else
        return self:format()
    end
end

local function funcEval(self, point)
    if self.actsOnExpressions then
        return self.func(self.parents[1])
    else
        local arg = self.parents[1]:evaluate(point)
        if type(arg) == "number" then
            return self.func(arg)
        else
            return self.funcWrapper(arg)
        end
    end
end
local function funcDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    return self.parents[1].derivative[withRespectTo] * self.funcDerivative(self.parents[1])
end
local function funcFormat(self)
    if type(self.repr) == "string" then
        return ("%s(%s)"):format(self.repr, tostring(self.parents[1]))
    else
        return self.repr(self.parents[1])
    end
end

local FuncWrapper = {}
local FuncWrapper__meta = {}
FuncWrapper__meta.__index = FuncWrapper

FuncWrapper__meta.__call = function(self, arg)
    if type(arg) == "number" then
        arg = M.const(arg)
    end
    local f = createBaseExpression(nodeTypes.func, funcEval, funcDerivative, funcFormat, {arg})
    f.repr = self.repr
    f.func = self.func
    f.funcWrapper = self
    f.funcDerivative = self.funcDerivative
    return f
end

---@param repr string|fun(Expression): string
---@param eval fun(arg: number): number
---@param actsOnExpressions boolean?
---@return Function
function M.func(repr, eval, actsOnExpressions)
    local wrapper = setmetatable({}, FuncWrapper__meta)
    wrapper.repr = repr
    wrapper.func = eval
    wrapper.actsOnExpressions = actsOnExpressions
    return wrapper
end

---@param deriv Function
function FuncWrapper:setDerivative(deriv)
    self.funcDerivative = deriv
end

M.identity = M.func("id", function(x) return x end, true)
M.reciproc = M.func(function(x) return ("1/(%s)"):format(x) end, function(x) return 1/x end, true)
M.sqrt = M.func("sqrt", function(x) return math.sqrt(x) end)
local sqrtDeriv = M.func(
    function(x) return ("1/(2*sqrt(%s))"):format(x) end,
    function(x) return 1/(2*M.sqrt(x)) end,
    true
)
M.sqrt:setDerivative(sqrtDeriv)

M.ln = M.func("ln", math.log)
M.ln:setDerivative(M.reciproc)

M.exp = M.func("exp", math.exp)
M.exp:setDerivative(M.exp)

local sinhF = function(x) return (M.exp(x) - M.exp(-x)) / 2 end
M.sinh = M.func("sinh", sinhF, true)
local coshF = function(x) return (M.exp(x) + M.exp(-x) / 2) end
M.cosh = M.func("cosh", coshF, true)

M.sinh:setDerivative(M.cosh)
M.cosh:setDerivative(M.sinh)

M.sin = M.func("sin", math.sin)
M.cos = M.func("cos", math.cos)
local msin = M.func("-sin", function(x) return -M.sin(x) end, true)
local mcos = M.func("-cos", function(x) return -M.cos(x) end, true)

M.sin:setDerivative(M.cos)
M.cos:setDerivative(msin)
msin:setDerivative(mcos)
mcos:setDerivative(M.sin)

return M