# SymDiff
A module for symbolic differentiation in Lua.

## General usage
SymDiff is designed to be used by creating variables and manipulating them via algebra or (wrapped) function calls. This allows the system to build an internal model of whatever function you intend to calculate, which in turn lets SymDiff create an expression that symbolically matches the original's derivative.

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
local temp = poly:evaluate {[x] = 2}  -- substitutes 2 in place of x, we get a new expression
print(temp)  -- prints "16 + 3*y^3 - 4*y"
print()

print(temp:evaluate(-1))  -- now that we're back to a single variable,
                          -- we can use the shortcut again. prints "17"

print(temp:evaluate {[y] = -1})  -- or we can always go back to the explicit notation
print(poly:evaluate {[x] = 2, [y] = -1})  -- prints "17" from poly in a single :evaluate call
print()

print(poly:derivative(x))  -- derivative with respect to x, prints "4*2*x - 2*y"
print(poly:derivative(y))  -- derivative with respect to y, prints "3*3*y^2 - 2*x"
print()

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
- `identity`: f(x) = 0
- `reciproc`: f(x) = 1/x
- `sqrt` (square root): f(x) = √x
- `ln` (natural log): f(x) = log_e(x)
- `exp` (exponential): f(x) = e^x
- `sin`, `cos`, `tan` (trigonometric functions)
- `sinh`, `cosh`, `tanh` (hyperbolic trigonometric functions)

But more importantly, the interface to create your own is available via `symdiff.func`. The first argument is the function being wrapped. It can be either a `function(Expression): Expression` (case I) or a `function(number): number` (case II). Expressions are any combination of `symdiff.func` and algebraic operators acting on variables and their composites. Essentially, if all your function does is manipulate a value algebrically, such as the reciprocal function, you have a case I function.

Case I functions are much nicer to work with, since SymDiff can infer most of the information about them, like how to differentiate them and how to display them.
For example, the reciprocal function can be defined as follows:

```lua
local reciproc = symdiff.func(
    function(x)
        return 1/x
    end
)
```

Now you can use `reciproc` with your variables. For example:

```lua
local z = symdiff.var("z")
local expr = 3 * reciproc(2*z^2)
print(expr)  --> prints "3 * 1/(2*z^2)"
```

Case II functions are usually mathematical library operations, such as `ln`. You can't define it algebraically, so there's no way to define a case I style function for it. Our only hope is a case II: wrapping a `function(number): number`. Let's look at an example:

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

Here we have 2 new pieces of information: the `repr` argument (usually the function name), and the `:setDerivative` call. Since SymDiff has no information on how the function is evaluated, we can provide the derivative in the shape of another `symdiff.func`. Note that this step is optional only if you don't take any derivatives
of your new `syndiff.func` (directly or indirectly, via the chain rule).

The third argument, `repr`, can be either a string, or a `function(Expression): string`. In the second case, it will be called with the function's argument every time it needs to be displayed. For example, if you wanted `exp` to show as `e^(x)`, you could do:

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

Here, `tostring` does all the heavy lifting for parsing the expression tree. All the `repr` function needs to do in this case is a simple `string.format`.

### Composing `symdiff.func`s
You can compose case I functions in any way you want. For example, with `symdiff.exp` we can define `sinh` and `cosh` as follows:

```lua
local sinh = symdiff.func(
    function(x)
        return (symdiff.exp(x) - symdiff.exp(-x)) / 2
    end
)
local cosh = symdiff.func(
    function(x)
        return (symdiff.exp(x) + symdiff.exp(-x)) / 2
    end
)

-- sinh:setDerivative(cosh)  --> optional! since they're case I functions,
-- cosh:setDerivative(sinh)      SymDiff can deduce the derivatives; however
--                               setting them will save some steps when computing
--                               them for the first time
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
- `isNumeric` (`function(value: any): boolean`)
- `isZero` (`function(value: Number): boolean`)
- `symdiff.ln` (`symdiff.func`)

Where "Number" denotes your custom numeric type. One or both of
`isNumeric` and `isZero` can be set by calling
`symdiff.setNumericChecks`. If the corresponding argument
is `nil`, that function is not updated.

`symdiff.ln`, however, is different: its value must be a set to a Function wrapper returned by `symdiff.func`. For example, this is the default implementation:

```lua
symdiff.ln = symdiff.func(math.log, true, "ln")
symdiff.reciproc = M.func(function(x) return 1/x end, true)
symdiff.ln:setDerivative(symdiff.reciproc)
```

`symdiff.ln` is important because it's used when computing derivatives of the form `f(x)^g(x)`.