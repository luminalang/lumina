# Partial Application

A large part of why functional programming is so powerful is being able to conveniently pass around functions as if they're values. 

This lets you create functions whose behavior is more generalised as a portion of the behavior can be passed down as a parameter. 

**The Closure Type**

Among types such as `int` or `(float, float)`, functions-as-values also have types in the form of closures. 

```lm
fn(int, int -> int)
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; The type of a closure which expects two `int` parameters and returns an `int`</sub></sup>

So for example; 

```lm
fn change_if_just f m as fn(int -> int), Maybe int -> Maybe int =
  match m
  | Nothing -> Nothing
  | Just n  -> Just (f n)
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A function which runs a function to change a value if it exists</sub></sup>

**The Magic `#` Unary Operator**

To treat a function as if it's a value, the `#` symbol is used. 

```lm
fn add_five_if_just m as Maybe int -> Maybe int =
  change_if_just #add_five m

fn add_five x as int -> int =
  x + 5
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A function which adds 5 to an integer if it exists</sub></sup>

The `#` symbol is a general-purpose way to pass various expressions as closures and can be used in a couple of different ways. 

```lm
fn add x y as int, int -> int = x + y

// ... //

// turns the function into a closure of type
// fn(int, int -> int)
#add

// partially applicates the function to create a closure with one less parameter
// fn(int -> int)
#(add 5)

// partially applicates an operator to create a closure with a single parameter
// fn(int -> int)
#(+ 5)

// turn a literal value into a closure
// fn( -> int), can also be written as fn(int)
#5
```
With this in mind, we can rewrite the previous example as:

```lm
fn add_five_if_just m as Maybe int -> Maybe int =
  change_if_just #(+ 5) m
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A function which adds 5 to an integer if it exists</sub></sup>


**Anonymous Functions with Lambdas**

If a function doesn't seem important enough to give it a name, we can inline it with a lambda. 

```lm
fn main =
  (\n -> n + 1) 5
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; Runs the inline function with the parameter `5` to create `6`</sub></sup>

Lambdas can also be passed as closures the same way as named functions. 

```lm
fn add_five_if_just m as Maybe int -> Maybe int =
  change_if_just #(\n -> n + 5) m
```

**Partially applicating Where-Bindings**

<sup>*TODO: Is it overly complicated and confusing to explain this here? Maybe we should have a separate design patterns chapter*</sup>

A common design pattern is to partially applicate where-bindings

```lm
fn add_five_if_just m as Maybe int -> Maybe int =
  change_if_just #(add 5)
 where
  fn add x y = x + y

  fn change_if_just f as fn(int -> int) -> Maybe int =
    match m
    | Nothing -> Nothing
    | Just n  -> Just (f n)
```
