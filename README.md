# Leaf - A High-Level Interpreted Functional Programming Language

## Introduction
Leaf's a work-in-progress programming language focused on efficiency of writing, readability, simplicity and performance.

## Full Design
```haskell
-- // This program attempts to display all the planned syntastic parts of the language
-- // Please note that since Leaf's still in pre-alpha this example will *NOT RUN*
-- // Syntax might also change in the future
-- // Check the examples folder for currently runnable examples of Leaf

use std:io

type person
    name string
    age  int
    data a

fn fmt p (person -> string)
    p.name <> " (" <> str p.age <> ")"

fn swap_data f p ((a -> a) person -> person)
    { p | data = f p.data }

fn prompt m (string)
    first io:puts m
    then  io:read_line

fn unwrap_or fallback x (a maybe<a> -> a)
    match x
      | Just v: v
      | None  : fallback
fn unwrap_or_else f x ((a) maybe<a> -> a)
    match x
      | Just v: v
      | None  : f

fn verify_age age (int -> int)
    if age < 0 or age > 200 
      then 18
      else n

fn main
    io:puts
        << swap_data #(\data: data + 1)
        << \p: { p | age = if p.age < 0 then 0 else p.age }
        << { person | name = prompt "What's your name?", age = age, data = 20 }
      where age = 
        verify_age << unwrap_or 18 << try_str (prompt "what's your age?")
```

## Features

### Data flow using '<<'
```haskell
-- Main returns '4'
fn main
    add 1 << add 1 << add 1 1
    
fn main
    add 1 (add 1 (add 1 1))
```

### User-defined operators
```
operator + (int int -> int)
    add left right

fn main
    4 + 2
```

### Powerful record syntax
```haskell
type point
    x int 
    y int

fn main
    -- modify the point coming from pipe
    { y = 3 }
    -- modify the point assign to 'p'
    << \p -> { p | x = 1 }
    -- initialize new instance of point
    << { point | x = 2, y = 1 }
```

### Function Parameters
```haskell
fn add x y (int int -> float)
    to_float << x + y

fn apply f x ((int -> float) int -> float) 
    f x

-- Main returns '10.0'
fn main
    -- The '#' shows intent to pass as function parameter. 
    -- Omitting it would cause a parameter amount mismatch since foo will be given to few arguments
    -- But with the '#' we convert it into an (int -> float) and pre-supply one parameter
    apply #(foo 5) 5

-- Main returns '10.0'
fn main
    -- Lambda expressions can also be turned into function parameters
    apply #(\n -> to_float << n + 4) 6

fn inc x (int -> int)
    x + 1

-- Main returns '10.0'
fn main
    -- If we're not looking to pre-supply any parameters
    -- (since the functions we're trying to pass already matches the expected function parameter)
    -- we can just use '#' on the identifier directly
    apply #inc 9
```

### Short and concise compile-time errors in plain human language
```
leaf /home/simon/C/rust/leaf/main.lf:
 2 |   add 4 [4]
      -^-
No function named `add` takes these parameters
  fn add (int [int] -> ...)
 i did however find these variants
  fn add (int int -> ...)
  fn add (float float -> ...)
```
```
leaf /home/simon/C/rust/leaf/main.lf:
 2 |   sum [1, 2
          -^-
This list open is missing a matching `]` to close it
```

### Modern rust-like error handling
```haskell
-- The 'try' keyword causes an early return upon encountering an None or Err variant
fn get_number_from_terminal (maybe<int>)
     \n -> Just n << try to_int << io:read_line
```
note: The try keyword might be replaced by early returns instead being fully implicit in the future, I haven't decided yet.

## Examples

Simply run them with `./leaf <leaf-file>`. \
If you don't want to install the leaf standard library to it's expected path you can use set the LEAFPATH environment variable, making the fully portable command `LEAFPATH=leafstd/ cargo run --release examples/<leaf-file>` \
Remember to compile Leaf using the `--release` flag! `cargo build --release` otherwise you'll get a whole lot of debug output and greatly degraded performance. 

## Status

The project is not yet in an usable state but we're getting there! 

 - [x] Index declarations for ease of access
 - [x] Design and implement the basic runner
 - [x] Implement int/string
 - [x] Create actually runnable Leaf programs
 - [x] Add a TON of assertions to improve error messages
 - [x] Implement all other primitive types (including lists)
 - [x] Build a (Rust -> Leaf) bridge
 - [ ] Write a low-level standard library in Rust using the bridge
 - [ ] Implement conversions between the primitive types
 - [x] Add logic operations (if, elif, else)
 - [x] Swap out string identifier to vec indexes at parse-time for huge performance boosts
 - [ ] Add { a | field = } and { field = } 
 - [ ] Add logic operations (match)
 - [ ] Add lambda support
 - [ ] Implement function to closure conversion using '#'
 - [ ] Implement custom types (structs/enums)
 - [x] Design and implement generics
 - [ ] Internally design and implement implicit result handling
 - [ ] Write Leaf's standard library abstracting over the Rust bridge
 - [ ] Add a proper CLI
 - [ ] Design and Implement multithreadding (green-threadded concurrent mapping is an idea)
 - [x] Implement stack-safe recursion
 - [x] Implement function overloading
