# Functions

## Declaring Functions

Functions are defined using the `fn` keyword. 

```lm
fn add x y as int int -> int =
  x + y
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A function which takes two parameters, x and y of type int, then adds them together to return a single int</sub></sup>

The type annotation is optional.

```lm
fn add x y = x + y
```
Although remember that type annotations often help as a form of documentation. 

## Calling Functions

White-space is used to separate the arguments to a function

```lm
fn main =
  add 1 2
```

Parenthesis can also be used to 'group' expressions. 

```lm
fn main =
  std:io:println (add (add 1 2) (add 3 4))
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A function adding numbers then printing them to the terminal</sub></sup>

## Pattern Parameters

The parameters in a function declaration may be any infallible pattern, not just plain identifiers. 

```lm
fn add_pairs xy ab as (int, int) (int, int) -> (int, int) =
  ...

// can instead be written as

fn add_pairs (x, y) (a, b) as (int, int) (int, int) -> (int, int) =
  (x + a, y + b)

fn main =
  std:io:println (add_pairs (1, 2) (3, 4))
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A function with two tuple parameters being pattern matched in the function declaration</sub></sup>

Read more about patterns in the [Pattern Matching & Conditionals](./matching.md) chapter

## Where-Bindings

A function may also be defined inside another function, of which it'll have access to its parent function's parameters. 

```lm
fn main =
  std:io:println (add 5 6)
 where
  fn add x y = x + y
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; A function declared inside another function as a where-binding</sub></sup>

## Operators

New operators can also be defined similarly to functions. 

```lm
fn +++ left right as int int -> int =
  left + right + right + right
```
