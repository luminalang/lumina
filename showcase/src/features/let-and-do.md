# Evaluating Multiple Expressions

### Let bindings

As a function grows larger, complex, or more and more nested. It can become difficult to read. 

```lm
fn main =
  add (add 1 2) (add 3 4)
```

A useful way to mitigate this is to seperate out the expressions into a sequence of steps. 
`let` allows you to bind the result of an expression to a pattern (often an identifier). 

```lm
fn main =
  let x = add 1 2 in
  let y = add 3 4 in
    add x y
```

This also comes with the benefit of allowing you to *name* the result of an expression, which is important for readability. 

The left side of a let binding can be any pattern, and not just an identifier. 

```lm
fn main =
  let (x, y) = (add 1 2, add 3 4)
   in add x y
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A let-binding with a tuple pattern binding to a tuple expression</sub></sup>

### Do expressions

Sometimes it's useful to run an expression for its side effects without using its return type. 
In those cases, `do...then` notation works similarly to `let` except it always discards the value instead of binding it to a pattern. 

```lm
fn main =
  // Run an expression
  do io:println "Starting Program..." then
    0   // Then do something else to generate a value

// ... Is a clearer way of writing

fn main =
  // Run an expression
  let _ = io:println "Starting Program..." in
    0   // Then do something else to generate a value
```
