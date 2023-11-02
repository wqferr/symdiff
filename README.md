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

## Documentation
Function signature documentation is mostly done via lua-language-server's type annotation system.
I would love to have both this and a static documentation page via LDoc, but alas they are not
compatible.

## Mathematical functions and how to implement your own
Currently, only a handful of functions are implemented ready for use:
- <code>identity</code>: f(x) = 0
- <code>reciproc</code>: f(x) = 1/x
- <code>sqrt</code> (square root): f(x) = √x
- <code>ln</code> (natural log): f(x) = log_e(x)
- <code>exp</code> (exponential): f(x) = e^x
- <code>sin</code>, <code>cos</code>, <code>tan</code> (trigonometric functions)
- <code>sinh</code>, <code>cosh</code>, <code>tanh</code> (hyperbolic trigonometric functions)

But more importantly, the interface to create your own is available via <code>symdiff.func</code>. The first argument is the function being wrapped. It can be either a <code>function(Expression): Expression</code> (case I) or a <code>function(number): number</code> (case II). Expressions are any combination of <code>symdiff.func</code> and algebraic operators operating on variables and their composites. Essentially, if all your function does is manipulate a value algebrically, such as the reciprocal function, you have a case I function.

Case I functions are much nicer to work with, since SymDiff can infer most of the information about them, like how to differentiate them and how to display them.
For example, the reciprocal function can be defined as follows:

```lua
local reciproc = symdiff.func(
    function(x)
        return 1/x
    end
)
```

Case II functions are usually mathematical library operations, such as <code>ln</code>. You can't define it algebraically, so there's no way to define a case I style function for it. Our only hope is a case II: wrapping a <code>function(number): number</code>. Let's look at an example:

```lua
local reciproc = symdiff.func(
    ... -- as above
)
local ln = symdiff.func(
    function(x)  -- takes a number, returns a number
        return math.log(x)
    end,
    true,  -- signal that to SymDiff
    "ln"  -- name of the function
)
ln:setDerivative(reciproc)
```

Here we have 2 new pieces of information: the <code>repr</code> argument (usually the function name), and the <code>:setDerivative</code> call. Since SymDiff has no information on how the function is evaluated, we can provide the derivative in the shape of another <code>symdiff.func</code>. Note that this step is optional only if you don't take any derivatives
of your new <code>syndiff.func</code> (directly or indirectly, via the chain rule).

The third argument, <code>repr</code>, can be either a string, or a <code>function(Expression): string</code>. In the second case, it will be called with the function's argument every time it needs to be displayed. For example, if you wanted <code>exp</code> to show as <code>e^(x)</code>, you could do:

```lua
local exp = symdiff.func(
    function(x)
        return math.exp(x)
    end,
    true,  -- flag function as numeric
    function(arg)
        return ("e^(%s)"):format(tostring(arg))
    end
)
```

Here, <code>tostring</code> does all the heavy lifting for parsing the expression tree. All the <code>repr</code> function needs to do in this case is a simple <code>string.format</code>.

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
symdiff.ln = symdiff.func(math.log, true, "ln")
symdiff.reciproc = M.func(function(x) return 1/x end, true)
symdiff.ln:setDerivative(symdiff.reciproc)
```