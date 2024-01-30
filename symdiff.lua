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

-- TODO make sum and product nodes support more than 2 parents
---@module "symdiff"
local M = {}
M._VERSION = "1.1.0"

-- Configuration region

---@alias sdNumeric number

---Checks if a value is sdNumeric
---@param value any
---@return boolean
local isNumeric = function(value)
    return type(value) == "number"
end

---Check if sdNumeric value is zero
---@param value sdNumeric
---@return boolean
local isZero = function(value)
    return value == 0
end
local zeroVal = 0

---Check if sdNumeric value is one
---@param value sdNumeric
---@return boolean
local isOne = function(value)
    return value == 1
end
local oneVal = 1

---Convert a number to a sdNumeric
---@param value number
---@return sdNumeric
local convert = function(value)
    return value
end

-- End of configuration region

---@diagnostic disable-next-line: deprecated
local unpack = unpack or table.unpack

local add, diff, mul, div, pow

---@class Expression
---@operator add(AlgebraicTerm): Expression
---@operator sub(AlgebraicTerm): Expression
---@operator mul(AlgebraicTerm): Expression
---@operator div(AlgebraicTerm): Expression
---@operator pow(AlgebraicTerm): Expression
---@operator unm(): Expression
---@field public derivative DerivativeAccessor
---@field package cachedDerivatives {Variable: Expression}
---@field package name string?
---@field package nodeType string
---@field package eval fun(self: Expression, point: Point): sdNumeric|Expression
---@field package format fun(self): string
---@field package dependencies {Variable: boolean}
---@field package calculateDerivative fun(self: Expression, variable: Variable)
---@field package parents Expression[]
---@field package cachedFormat string?
M.Expression = {}
local Expression__meta = {}
Expression__meta.__index = M.Expression

---@class Variable: Expression
---@operator add(AlgebraicTerm): Expression
---@operator sub(AlgebraicTerm): Expression
---@operator mul(AlgebraicTerm): Expression
---@operator div(AlgebraicTerm): Expression
---@operator pow(AlgebraicTerm): Expression
---@operator unm(): Expression

---@class Function
---@operator call(AlgebraicTerm): Expression
---@field public func WrappedFunction
---@field package funcDerivative Function
---@field package actsOnExpressions boolean?
---@field package repr (string|fun(arg: Expression): string)?
local FuncWrapper = {}
local FuncWrapper__meta = {}

---@class FunctionCall: Expression
---@field package repr (string|fun(arg: Expression): string)?
---@field package funcWrapper table?
---@field package func fun(arg: sdNumeric): sdNumeric
---@field package funcDerivative Function
---@field package actsOnExpressions boolean?

---@alias Point
---|{Variable: sdNumeric} a mapping for the value of each variable at the point
---|number the value of the single Variable of the Expression

---@alias AlgebraicTerm Variable|Expression|sdNumeric

---@alias WrappedFunction (fun(arg: sdNumeric): sdNumeric)|(fun(arg: Expression): Expression)

local zero, one

---@type Point
-- Used for evaluating constants without littering memory space
local nullPoint = {}

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
setmetatable(nodeTypes, {__index = function(_, k) error("Unknown nodeType: "..tostring(k)) end})

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

---@class DerivativeAccessor
---@operator call(Variable): Expression
---@operator call(Point): Expression|sdNumeric
---@field expression Expression
local DerivativeAccessor__meta = {}

DerivativeAccessor__meta.__call = function(accessor, expression, arg)
    if type(arg) == "table" and arg.nodeType == nodeTypes.var then
        if not accessor[arg] then
            ---@cast arg Variable
            if expression.dependencies[arg] then
                accessor[arg] = expression:calculateDerivative(arg)
            else
                accessor[arg] = zero
            end
        end
        return accessor[arg]
    else
        ---@cast arg Point
        local onlyDep, reason = getOnlyDependency(expression)
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
        if arg then
            return accessor(expression, onlyDep):evaluate(arg)
        else
            return accessor(expression, onlyDep)
        end
    end
end

---Get the priority value of a given Expression node
---@param expression Expression
---@return sdNumeric
---@nodiscard
local function getPriority(expression)
    return priorities[expression.nodeType]
end

---Create a new derivative accessor for the given Expression
---@return DerivativeAccessor
---@nodiscard
local function createDerivativeAccessor()
    local accessor = setmetatable({}, DerivativeAccessor__meta)
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

local function errorIfAnyIsNull(n, ...)
    local args = {...}
    for idx = 1, n do
        if args[idx] == nil then
            error("Expected non-nil value for operand #"..idx, 3)
        end
    end
end

---Check whether expr is a constant node or not
---@param expr Expression the expression to check
---@return boolean const true if and only if expr is a constant node
local function isConstant(expr)
    if expr == nil then
        error("Unexpected nil value", 2)
    end
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
---@param eval fun(e: Expression, p: Point): Expression|sdNumeric evaluation function based on the Expression's parents
---@param derivative fun(e: Expression, withRespectTo: Variable): Expression evaluation function for the expression's derivative based on its parents
---@param format fun(e: Expression): string function for representing the Expression as a string
---@param parents Expression[] parents of the expression, usually the operands
---@return Expression expr the newly created Expression node
---@nodiscard
local function createBaseExpression(nodeType, eval, derivative, format, parents)
    local expr = setmetatable({}, Expression__meta)
    expr.nodeType = nodeType
    expr.derivative = createDerivativeAccessor()
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
---@return sdNumeric|Expression result a number if there were no unresolved dependencies, an Expression of said dependencies otherwise
function M.Expression:evaluate(point)
    if isNumeric(point) then
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
    if type(result) == "number" then
        result = convert(result)
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

---@param name string the name of the variable
---@return Variable
function M.var(name)
    assert(type(name) == "string", "Variables must have a name")
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

local function constDerivative(_, _)
    return zero
end

local function constFormat(self)
    if self.name then
        return self.name
    else
        return tostring(self:evaluate(nullPoint))
    end
end

---Wrap a constant value for use in SymDiff
---@param value any
---@param name string? name of the constant
---@return Expression
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
zero = M.const(zeroVal)
one = M.const(oneVal)

local function sumEval(self, point)
    return self.parents[1]:evaluate(point) + self.parents[2]:evaluate(point)
end
local function sumDerivative(self, withRespectTo)
    return self.parents[1]:derivative(withRespectTo) +
        self.parents[2]:derivative(withRespectTo)
end
local function sumFormat(self)
    return ("%s + %s"):format(tostring(self.parents[1]), tostring(self.parents[2]))
end

function add(a, b)
    errorIfAnyIsNull(2, a, b)
    if isNumeric(a) then
        a = M.const(a)
    end
    if isNumeric(b) then
        b = M.const(b)
    end
    if isConstant(b) then
        a, b = b, a
    end
    if isConstant(a) then
        local aEval = a:evaluate(nullPoint)
        ---@cast aEval sdNumeric
        if isZero(aEval) then
            return b
        elseif isConstant(b) then
            return M.const(aEval + b:evaluate(nullPoint))
        end
    end
    return createBaseExpression(nodeTypes.sum, sumEval, sumDerivative, sumFormat, {a, b})
end
Expression__meta.__add = function(a, b)
    return add(a, b)
end

local function diffEval(self, point)
    return self.parents[1]:evaluate(point) - self.parents[2]:evaluate(point)
end
local function diffDerivative(self, withRespectTo)
    return self.parents[1]:derivative(withRespectTo) -
        self.parents[2]:derivative(withRespectTo)
end
local function diffFormat(self)
    return ("%s - %s"):format(tostring(self.parents[1]), tostring(self.parents[2]))
end
function diff(a, b)
    errorIfAnyIsNull(2, a, b)
    if isNumeric(a) then
        a = M.const(a)
    end
    if isNumeric(b) then
        b = M.const(b)
    end
    if isConstant(a) then
        local aEval = a:evaluate(nullPoint)
        ---@cast aEval sdNumeric
        if isZero(aEval) then
            return -b
        elseif isConstant(b) then
            return M.const(aEval - b:evaluate(nullPoint))
        end
    else
        if isConstant(b) and isZero(b:evaluate(nullPoint) --[[@as sdNumeric]]) then
            return a
        end
    end
    return createBaseExpression(
        nodeTypes.diff,
        diffEval,
        diffDerivative,
        diffFormat,
        {a, b}
    )
end
Expression__meta.__sub = function(a, b)
    return diff(a, b)
end

local function unmEval(self, point)
    return -self.parents[1]:evaluate(point)
end
local function unmDerivative(self, withRespectTo)
    return -self.parents[1]:derivative(withRespectTo)
end
local function unmFormat(self)
    local p1 = wrapParentsIfNeeded(self, self.parents)
    return ("-%s"):format(tostring(p1))
end
Expression__meta.__unm = function(a)
    if isNumeric(a) then
        return M.const(-a)
    end
    return createBaseExpression(
        nodeTypes.unm,
        unmEval,
        unmDerivative,
        unmFormat,
        {a}
    )
end

local function productEval(self, point)
    return self.parents[1]:evaluate(point) * self.parents[2]:evaluate(point)
end
local function constProductDerivative(self, withRespectTo)
    return self.parents[1] * self.parents[2]:derivative(withRespectTo)
end
local function productDerivative(self, withRespectTo)
    local p1, p2 = self.parents[1], self.parents[2]
    return p1*p2:derivative(withRespectTo) + p1:derivative(withRespectTo)*p2
end
local function productFormat(self)
    local p1, p2 = wrapParentsIfNeeded(self, self.parents)
    return ("%s * %s"):format(p1, p2)
end
local function constProduct(const, expr)
    assert(isConstant(const), "First argument to constProduct must be a Constant")
    return createBaseExpression(
        nodeTypes.mul,
        productEval,
        constProductDerivative,
        productFormat,
        {const, expr}
    )
end
function mul(a, b)
    errorIfAnyIsNull(2, a, b)
    if isNumeric(a) then
        a = M.const(a)
    end
    if isNumeric(b) then
        b = M.const(b)
    end
    if isConstant(b) then
        a, b = b, a
    end
    if isConstant(a) then
        local aEval = a:evaluate(nullPoint) --[[@as sdNumeric]]
        if isOne(aEval) then
            return b
        elseif isConstant(b) then
            return M.const(aEval * b:evaluate(nullPoint))
        elseif isZero(aEval) then
            return zero
        else
            return constProduct(a, b)
        end
    end
    return createBaseExpression(
        nodeTypes.mul,
        productEval,
        productDerivative,
        productFormat,
        {a, b}
    )
end
Expression__meta.__mul = function(a, b)
    return mul(a, b)
end

local function quotientEval(self, point)
    return self.parents[1]:evaluate(point) / self.parents[2]:evaluate(point)
end
local function quotientDerivative(self, withRespectTo)
    local a, b = self.parents[1], self.parents[2]
    return (a:derivative(withRespectTo) * b - a * b:derivative(withRespectTo)) / (b^2)
end
local function quotientFormat(self)
    local p1, p2 = wrapParentsIfNeeded(self, self.parents)
    return ("%s / %s"):format(p1, p2)
end
function div(a, b)
    errorIfAnyIsNull(2, a, b)
    if isNumeric(a) then
        a = M.const(a)
    end
    if isNumeric(b) then
        assert(not isZero(b), "Division by 0")
        return (1/b) * a
    elseif isConstant(b) then
        local bEval = b:evaluate(nullPoint)
        assert(not isZero(bEval), "Division by 0")
        if isConstant(a) then
            return M.const(a:evaluate(nullPoint) / bEval)
        else
            return (1/bEval) * a
        end
    end
    return createBaseExpression(
        nodeTypes.div,
        quotientEval,
        quotientDerivative,
        quotientFormat,
        {a, b}
    )
end
Expression__meta.__div = function(a, b)
    return div(a, b)
end

local function powerEval(self, point)
    return self.parents[1]:evaluate(point) ^ self.parents[2]:evaluate(point)
end
local function powerRuleDerivative(self, withRespectTo)
    assert(isConstant(self.parents[2]), "Power rule only works for constant exponents")
    local result = self.parents[2] * self.parents[1] ^ (self.parents[2] - 1) * self.parents[1]:derivative(withRespectTo)
    return result
end
local function constantBasePowerDerivative(self, withRespectTo)
    assert(isConstant(self.parents[1]), "Constant base power derivative requires constant base")
    local result = M.ln(self.parents[1]) *
        (self.parents[1]^self.parents[2]) *
        self.parents[2]:derivative(withRespectTo)
    return result
end
local function generalPowerDerivative(self, withRespectTo)
    -- d/dx (f(x)^g(x)) =
    -- f(x)^g(x) * (g'(x)ln(f(x)) + (f'(x)g(x))/f(x))
    local result = (self.parents[1] ^ self.parents[2]) *
        (
            self.parents[2]:derivative(withRespectTo)*M.ln(self.parents[1]) +
            (self.parents[1]:derivative(withRespectTo)*self.parents[2]) / self.parents[1]
        )
    return result
end
local function powerFormat(self)
    local p1, p2 = wrapParentsIfNeeded(self, self.parents)
    return ("%s^%s"):format(p1, p2)
end
function pow(a, b)
    errorIfAnyIsNull(2, a, b)
    if isNumeric(a) then
        a = M.const(a)
    end
    if isNumeric(b) then
        b = M.const(b)
    end
    local calculateDerivative
    if isConstant(b) then
        if isZero(b:evaluate(nullPoint) --[[@as sdNumeric]]) then
            return one
        elseif isOne(b:evaluate(nullPoint) --[[@as sdNumeric]]) then
            return a
        end
        calculateDerivative = powerRuleDerivative
    elseif isConstant(a) then
        if isZero(a:evaluate(nullPoint) --[[@as sdNumeric]]) then
            return zero
        end
        calculateDerivative = constantBasePowerDerivative
    else
        calculateDerivative = generalPowerDerivative
    end
    return createBaseExpression(
        nodeTypes.pow,
        powerEval,
        calculateDerivative,
        powerFormat,
        {a, b}
    )
end
Expression__meta.__pow = function(a, b)
    return pow(a, b)
end

local function funcEval(self, point)
    errorIfAnyIsNull(1, point)
    if self.actsOnExpressions then
        return self.func(self.parents[1]):evaluate(point)
    else
        local arg = self.parents[1]:evaluate(point)
        if isNumeric(arg) then
            return self.func(arg)
        else
            return self.funcWrapper(arg)
        end
    end
end
local function funcDerivative(self, withRespectTo)
    local selfDeriv
    if self.funcDerivative then
        selfDeriv = self.funcDerivative(self.parents[1])
    elseif self.actsOnExpressions then
        selfDeriv = self.func(self.parents[1]):derivative(withRespectTo)
    else
        error("No known method for computing derivative of user-specified function")
    end
    return self.parents[1]:derivative(withRespectTo) * selfDeriv
end
local function funcFormat(self)
    if type(self.repr) == "string" then
        return ("%s(%s)"):format(self.repr, tostring(self.parents[1]))
    elseif self.repr then
        return self.repr(self.parents[1])
    elseif self.actsOnExpressions then
        return tostring(self.func(self.parents[1]))
    else
        error("Function passed nil to repr argument without specifying actsOnExpressions: cannot be displayed")
    end
end

Expression__meta.__tostring = function(self)
    if not self.cachedFormat then
        self.cachedFormat = self:format()
    end
    return self.cachedFormat
end

FuncWrapper__meta.__index = FuncWrapper

FuncWrapper__meta.__call = function(self, arg)
    if isNumeric(arg) then
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

---@param eval WrappedFunction the function to wrap
---@param numeric boolean? whether the function deals with numbers (true) or Expressions (false, default)
---@param repr (string|fun(Expression): string)? name or formatting function for this Function
---@return Function
function M.func(eval, numeric, repr)
    local wrapper = setmetatable({}, FuncWrapper__meta)
    wrapper.func = eval
    wrapper.actsOnExpressions = not numeric
    wrapper.repr = repr
    return wrapper
end

---Set the derivative calculating Function for this Function.
---This is recommended only for Functions flagged as numeric.
---@param deriv Function the function whose evaluation equals self's derivative
function FuncWrapper:setDerivative(deriv)
    self.funcDerivative = deriv
end

M.identity = M.func(function(x) return x end)
M.reciproc = M.func(function(x) return 1/x end)

M.sqrt = M.func(function(x) return math.sqrt(x --[[@as sdNumeric]]) end, true, "sqrt")
local sqrtDeriv = M.func(function(x) return 1/(2*M.sqrt(x)) end)
M.sqrt:setDerivative(sqrtDeriv)

M.ln = M.func(math.log, true, "ln")
M.ln:setDerivative(M.reciproc)

M.exp = M.func(math.exp, true, "exp")
M.exp:setDerivative(M.exp)

local sinhF = function(x) return (M.exp(x) - M.exp(-x)) / 2 end
M.sinh = M.func(sinhF, false, "sinh")
local coshF = function(x) return (M.exp(x) + M.exp(-x) / 2) end
M.cosh = M.func(coshF, false, "cosh")
local tanhF = function(x) return (M.exp(2*x) - 1) / (M.exp(2*x) + 1) end
M.tanh = M.func(tanhF, false, "tanh")
local tanhDeriv = M.func(function(x) return 1/M.cosh(x)^2 end, true)
M.tanh:setDerivative(tanhDeriv)

M.sinh:setDerivative(M.cosh)
M.cosh:setDerivative(M.sinh)

M.sin = M.func(math.sin, true, "sin")
M.cos = M.func(math.cos, true, "cos")
M.tan = M.func(math.tan, true, "tan")
local msin = M.func(function(x) return -M.sin(x) end)
local mcos = M.func(function(x) return -M.cos(x) end)
local sec2inv = M.func(function(x) return 1/(M.cos(x))^2 end)

M.sin:setDerivative(M.cos)
M.cos:setDerivative(msin)
msin:setDerivative(mcos)
mcos:setDerivative(M.sin)
M.tan:setDerivative(sec2inv)

return M
