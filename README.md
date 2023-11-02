# SymDiff
A module for symbolic differentiation in Lua.

## General usage
SymDiff is designed to be used by creating Variables and manipulating them via algebra or (wrapped) function calls. This allows the system to build an internal model of whatever function you intend to calculate, which in turn lets SymDiff create an expression that symbolically matches the original's derivative.

For example, take this simple example:

```lua
local sd = require("symdiff")
local x = sd.var("x")  -- this creates the object we will manipulate
local poly = x^2 - x
print(poly)  -- prints "x^2 - x"
print(poly:evaluate(5))  -- prints "20"
print()

print(poly:derivative())  -- prints "2*x - 1"
print(poly:derivative():evaluate(2))  -- prints "3"
print(poly:derivative(2))  -- shortcut, same as the line above
```

SymDiff is able to manipulate variables and function calls to deduce what the
derivative of the expression should be. It can even handle multiple variables!

```lua
local sd = require("symdiff")
local x = sd.var("x")
local y = sd.var("y")
local poly = 4*x^2 + 3*y^3 - 2*x*y
-- print(poly:evaluate(3))  -- wrong! with multiple variables, this shortcut errors!

-- notice the curly braces in the following lines!
local temp = poly:evaluate{[x] = 2}  -- substitutes 2 in place of x, we get a new expression
print(temp)  -- prints "16 + 3*y^3 - 4*y"

print(temp:evaluate(-1))  -- now that we're back to a single variable,
                          -- we can use the shortcut again. prints "17"

print(temp:evaluate {[y] = -1})  -- or we can always go back to the explicit notation

print(poly:evaluate {[x] = 2, [y] = -1})  -- prints "17" from poly in a single :evaluate call

print(poly:derivative(x))  -- derivative with respect to x, prints "4*2*x - 2*y"
print(poly:derivative(y))  -- derivative with respect to y, prints "3*3*y^2 - 2*x"
-- print(poly:derivative(x, y))  -- wrong!
local ddx = poly:derivative(x)  -- previous result was cached, no worries about performance
local ddx_ddy = ddx:derivative(y)
print(ddx_ddy)  -- prints "-2"
```

## Limitations
SymDiff is not designed to be performant, although some naïve attempts at
simplifying algebraic expressions were done.

It also currently only supports single valued, single argument functions.
Multivariate calculus can still be modeled by creating a number of variables
and relating them to one another outside the library.

## Usage with different numeric systems
This module supports different numeric systems than that of Lua.
For that, it exposes 3 functions that can be replaced as the user wishes:
- <code>isNumeric</code> (<code>function(value: any): boolean</code>)
- <code>isZero</code> (<code>function(value: Number): boolean</code>)
- <code>symdiff.ln</code> (<code>symdiff.func</code>)

Where "Number" denotes your custom numeric type. One or both of
<code>isNumeric</code> and <code>isZero</code> can be set by calling
<code>symdiff.setNumericChecks</code>. If the corresponding argument
is <code>nil</code>, that function is not updated.

<code>symdiff.ln</code>, however, is different: its value must be a set to a Function wrapper returned by <code>symdiff.func</code>. For example, this is the default implementation:

```lua
symdiff.ln = symdiff.func(math.log, false, "ln")
symdiff.reciproc = M.func(function(x) return 1/x end, true)
symdiff.ln:setDerivative(symdiff.reciproc)
```