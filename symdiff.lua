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

--- <h2>A module for symbolic differentiation in Lua</h2>
-- @module symdiff
-- @alias M
-- @release 1.0.0
-- @author William Quelho Ferreira
-- @copyright 2022
-- @license MIT
---
local M = {}
M._VERSION = "1.0.0"


---@diagnostic disable-next-line: deprecated
local unpack = unpack or table.unpack


---@class Variable: Expression

---@class Expression
---@field cachedDerivatives {Variable: Expression}
---@field name string?
---@field nodeType string
---@field eval fun(self: Expression, point: Point): number|Expression
---@field format fun(self): string
---@field dependencies {Variable: boolean}
---@field calculateDerivative fun(self: Expression, variable: Variable)
---@field parents Expression[]
---@field derivative DerivativeAccessor
---@field cachedFormat string?
M.Expression = {}
local Expression__meta = {}
Expression__meta.__index = M.Expression

---@class Function
---@field name string
---@field func fun(arg: number): number
---@field funcDerivative Function
---@field actsOnExpressions boolean?
---@field setDerivative fun(self: Function, arg: Function)
---@field repr (string|fun(arg: Expression): string)?
local FuncWrapper = {}
local FuncWrapper__meta = {}

---@class FunctionCall: Expression
---@field repr (string|fun(arg: Expression): string)?
---@field funcWrapper table?
---@field func fun(arg: number): number
---@field funcDerivative Function
---@field actsOnExpressions boolean?

---@alias Point
---|{Variable: number} a mapping for the value of each variable at the point
---|number the value of the single variable of the Expression

local zero, one

---@type Point
-- Used for evaluating constants
local nullPoint = {}


---Get this Expression's dependency, if is the only dependency
---@param expr Expression
---@return Variable? dep the dependency, if single, nil otherwise
---@return string? reason the reason a single dependency couldn't be fetched
local function getOnlyDependency(expr)
    local onlyDep = next(expr.dependencies)
    if not onlyDep then
        return nil, "expression is constant"
    elseif next(expr.dependencies, onlyDep) then
        return nil, "multiple dependencies"
    else
        return onlyDep
    end
end

---@class DerivativeAccessor: {Variable: Expression}
---@field expression Expression
local DerivativeAccessor__meta = {}

---@param t DerivativeAccessor
---@param k Variable variable with respect to which to differentiate
---@nodiscard
DerivativeAccessor__meta.__index = function(t, k)
    if not rawget(t, k) then
        t[k] = t.expression:calculateDerivative(k)
    end
    return t[k]
end
---@param t DerivativeAccessor
---@param point Point the point at which to evaluate the derivative
---@nodiscard
DerivativeAccessor__meta.__call = function(t, point)
    local onlyDep, reason = getOnlyDependency(t.expression)
    if not onlyDep then
        ---@cast reason string
        if reason:find "constant" then
            return zero
        elseif reason:find "multiple" then
            error("Cannot call derivative without index in expression with multiple dependencies")
        else
            error("An unknown error has occurred")
        end
    end
    return t[onlyDep]:evaluate(point)
end

---@enum nodeTypes
local nodeTypes = {
    const = "constant",
    var = "variable",
    sum = "sum",
    diff = "difference",
    unm = "negation",
    mul = "product",
    div = "division",
    pow = "power",
    func = "function"
}
setmetatable(nodeTypes, {__index = function(t, k) error("Unknown nodeType: "..tostring(k)) end})

---@enum priorities
local priorities = {
    [nodeTypes.func] = 11,
    [nodeTypes.const] = 10,
    [nodeTypes.var] = 10,
    [nodeTypes.unm] = 9,
    [nodeTypes.pow] = 8,
    [nodeTypes.div] = 3,
    [nodeTypes.mul] = 2,
    [nodeTypes.diff] = 1,
    [nodeTypes.sum] = 1,
}
---Get the priority value of a given Expression node
---@param expression Expression
---@return number
---@nodiscard
local function getPriority(expression)
    return priorities[expression.nodeType]
end

---Create a new derivative accessor for the given Expression
---@param expression Expression
---@return DerivativeAccessor
---@nodiscard
local function createDerivativeAccessor(expression)
    local accessor = setmetatable({}, DerivativeAccessor__meta)
    accessor.expression = expression
    return accessor
end

---@param dependent Expression
---@param dependencyVariable Variable
local function addDependency(dependent, dependencyVariable)
    dependent.dependencies[dependencyVariable] = true
end

---Merge two maps, prioritizing the new keys
---@param onto table
---@param new table
local function merge(onto, new)
    for k, v in pairs(new) do
        onto[k] = v
    end
end

---Check whether expr is a constant node or not
---@param expr Expression the expression to check
---@return boolean const true if and only if expr is a constant node
local function isConstant(expr)
    return expr.nodeType == nodeTypes.const
end

---Wrap parents of the Expression expr in parens if needed
---@param expr Expression the expression being evaluated as a string
---@param parents Expression[] operands of the given Expression
---@return string ... tostring(p) for every p in parents, wrapped with parentheses if necessary
local function wrapParentsIfNeeded(expr, parents)
    local results = {}
    local ps = getPriority(expr)
    for _, p in ipairs(parents) do
        local s = tostring(p)
        if getPriority(p) < ps then
            s = ("(%s)"):format(s)
        end
        table.insert(results, s)
    end
    return unpack(results)
end

---Create a new Expression node
---@param nodeType nodeTypes type of the node being created
---@param eval fun(e: Expression, p: Point): Expression|number evaluation function based on the Expression's parents
---@param derivative fun(e: Expression, withRespectTo: Variable): Expression evaluation function for the expression's derivative based on its parents
---@param format fun(e: Expression): string function for representing the Expression as a string
---@param parents Expression[] parents of the expression, usually the operands
---@return Expression expr the newly created Expression node
---@nodiscard
local function createBaseExpression(nodeType, eval, derivative, format, parents)
    local expr = setmetatable({}, Expression__meta)
    expr.nodeType = nodeType
    expr.derivative = createDerivativeAccessor(expr)
    expr.dependencies = {}
    expr.cachedFormat = nil
    expr.parents = parents
    expr.eval = eval
    expr.calculateDerivative = derivative
    expr.format = format
    for _, parent in ipairs(parents) do
        merge(expr.dependencies, parent.dependencies)
    end
    return expr
end

---Evaluate the Expression at a given point
---@param point Point the point at which to evaluate the Expression
---@return number|Expression result a number if there were no unresolved dependencies, an Expression of said dependencies otherwise
function M.Expression:evaluate(point)
    if type(point) == "number" then
        local onlyDep, reason = getOnlyDependency(self)
        ---@cast reason string
        if onlyDep then
            point = {[onlyDep] = point}
        elseif reason:find "constant" then
            point = nullPoint
        elseif reason:find "multiple" then
            error("Cannot use implicit variable points when there's multiple dependencies")
        else
            error("An unknown error has occurred")
        end
    end
    local result = self:eval(point)
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
local function varDerivative(self, withRespectTo)
    if self == withRespectTo then
        return one
    else
        return zero
    end
end
local function varFormat(self)
    return self.name
end

---@param name string
---@return Variable
function M.var(name)
    ---@type Variable
    ---@diagnostic disable-next-line: assign-type-mismatch
    local v = createBaseExpression(
        nodeTypes.var,
        varEval,
        varDerivative,
        varFormat,
        {}
    )
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
    local c = createBaseExpression(
        nodeTypes.const,
        createConstEval(value),
        constDerivative,
        constFormat,
        {}
    )
    c.name = name
    return c
end
zero = M.const(0)
one = M.const(1)

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

local function diffEval(self, point)
    return self.parents[1]:evaluate(point) - self.parents[2]:evaluate(point)
end
local function diffDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    return self.parents[1].derivative[withRespectTo] -
        self.parents[2].derivative[withRespectTo]
end
local function diffFormat(self)
    return ("%s - %s"):format(tostring(self.parents[1]), tostring(self.parents[2]))
end
Expression__meta.__sub = function(a, b)
    if type(a) == "number" then
        a = M.const(a)
    end
    if type(b) == "number" then
        b = M.const(b)
    end
    if isConstant(a) then
        if a:evaluate(nullPoint) == 0 then
            return -b
        elseif isConstant(b) then
            return M.const(a:evaluate(nullPoint) - b:evaluate(nullPoint))
        end
    elseif isConstant(b) and b:evaluate(nullPoint) == 0 then
        return a
    end
    local diff = createBaseExpression(nodeTypes.diff, diffEval, diffDerivative, diffFormat, {a, b})
    return diff
end

local function unmEval(self, point)
    return -self.parents[1]:evaluate(point)
end
local function unmDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    return -self.parents[1].derivative[withRespectTo]
end
local function unmFormat(self)
    local p1 = wrapParentsIfNeeded(self, self.parents)
    return ("-%s"):format(tostring(p1))
end
Expression__meta.__unm = function(a)
    if type(a) == "number" then
        return M.const(-a)
    end
    return createBaseExpression(nodeTypes.unm, unmEval, unmDerivative, unmFormat, {a})
end

local function productEval(self, point)
    return self.parents[1]:evaluate(point) * self.parents[2]:evaluate(point)
end
local function constProductDerivative(self, withRespectTo)
    if not self.dependencies[withRespectTo] then
        return zero
    end
    return self.parents[1]:evaluate(nullPoint) *
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
    elseif isConstant(b) and b:evaluate(nullPoint) == 1 then
        return a
    end
    local product = createBaseExpression(nodeTypes.mul, productEval, productDerivative, productFormat, {a, b})
    return product
end

local function quotientEval(self, point)
    return self.parents[1]:evaluate(point) / self.parents[2]:evaluate(point)
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
    return result
end
local function constantBasePowerDerivative(self, withRespectTo)
    assert(isConstant(self.parents[1]), "Constant base power derivative requires constant base")
    if not self.dependencies[withRespectTo] then
        return zero
    end
    local result = math.log(self.parents[1]:evaluate(nullPoint)) *
        (self.parents[1]^self.parents[2]) *
        self.parents[2].derivative[withRespectTo]
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
            return one
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

local function funcEval(self, point)
    if self.actsOnExpressions then
        return self.func(self.parents[1]):evaluate(point)
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
    local selfDeriv
    if self.funcDerivative then
        selfDeriv = self.funcDerivative(self.parents[1])
    elseif self.actsOnExpressions then
        selfDeriv = self.func(self.parents[1]).derivative[withRespectTo]
    else
        error("No known method for computing derivative of user-specified function")
    end
    return self.parents[1].derivative[withRespectTo] * selfDeriv
end
local function funcFormat(self)
    if type(self.repr) == "string" then
        return ("%s(%s)"):format(self.repr, tostring(self.parents[1]))
    elseif self.repr then
        return self.repr(self.parents[1])
    elseif self.actsOnExpressions then
        return tostring(self.func(self.parents[1]))
    else
        error("Function passed nil to repr argument without specifying actsOnExpressions")
    end
end

Expression__meta.__tostring = function(self)
    if not self.cachedFormat then
        self.cachedFormat = self:format()
    end
    return self.cachedFormat
end

FuncWrapper__meta.__index = FuncWrapper

---kfldsjflkasldf
---@return Expression
FuncWrapper__meta.__call = function(self, arg)
    if type(arg) == "number" then
        arg = M.const(arg)
    end
    ---@type FunctionCall
    ---@diagnostic disable-next-line: assign-type-mismatch
    local f = createBaseExpression(nodeTypes.func, funcEval, funcDerivative, funcFormat, {arg})
    f.actsOnExpressions = self.actsOnExpressions
    f.repr = self.repr
    f.func = self.func
    f.funcWrapper = self
    f.funcDerivative = self.funcDerivative
    return f
end

---@param repr (string|fun(Expression): string)? if nil and actsOnExpressions, evaluates it for the format Expression
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

M.identity = M.func(nil, function(x) return x end, true)
M.reciproc = M.func(function(x) return ("1/(%s)"):format(x) end, function(x) return 1/x end, true)
M.sqrt = M.func("sqrt", function(x) return math.sqrt(x) end)
local sqrtDeriv = M.func(
    nil,
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
M.tan = M.func("tan", math.tan)
local msin = M.func(nil, function(x) return -M.sin(x) end, true)
local mcos = M.func(nil, function(x) return -M.cos(x) end, true)
local sec2inv = M.func(nil, function(x) return 1/(M.cos(x))^2 end, true)

M.sin:setDerivative(M.cos)
M.cos:setDerivative(msin)
msin:setDerivative(mcos)
mcos:setDerivative(M.sin)
M.tan:setDerivative(sec2inv)

return M