# Leaf - A High-Level Interpreted Functional Programming Language

## Introduction
Leaf's a work-in-progress programming language focused on efficiency of writing, readability, simplicity and performance.

## Full Design
```haskell
-- || This program attempts to display all the planned syntastic parts of the language
-- || Please note that since Leaf's still in pre-alpha this example will *NOT RUN*
-- || Syntax might also change in the future
-- || Check the examples folder for currently runnable examples of Leaf

use std:io

type person
    name :: string
    age  :: int
    data :: a

fn fmt p (person -> string)
    p.name <> " (" <> str p.age <> ")"

fn swap_data f p ((a -> a) P -> P)
        where P: person
    { p | data = f p.data }

fn get_name (string)
    prompt "What's your name? "

fn get_age (maybe<int>)
    try_str << prompt "What's your age? "

fn prompt m (string)
    first io:puts m
    then  io:read_line

fn main
    io:puts
        << fmt 
        << swap_data #(\data: data + 1)
        << \p: { p | age = if p.age < 0 then 0 else p.age }
        << { person | name = get_name, age = age, data = 20 }
      where age = 
        match get_age 
          | Just a: a
          | None  : 18
```

## Features

### Syntax of Haskell
Even though the philosophy of Leaf and Haskell is drastically different, the syntax is relatively similar generally giving the same feel.\
There's no variables, you heavily rely on lambda expressions, your program is built from small functions that grow outwards. 

### Simplicity 
Removing features is often more desired than adding features. Because as the feature set increases, readability and simplicity decreases.

### Efficiency
Maintaining simplicity is for the wrong cause if it hurts efficiency.\
Leaf attempts to minimize boilerplate and generally reduce the parts of programming that is often found to be annoying by you describing behavior to add extra implicit functionality.

### The << Pipe Operator
Data is most of the time separated by the `<<` operator, and using the `<<` operator should always be preferred over lisp-style parenthesis.\

If you're familiar with the `<|` operator from Elixir/Elm/F# or the `$` operator from Haskell, then getting used to this new style of data flow will be no problem. 

### Concurrency
*(not yet implemented)*
A great system for concurrency is the language's job to internally implement. Although a few draft designs exists it's to early to fully implement concurrency. 

### Error / Failure Handling
Safety and Purity are good things, but shouldn't be overdone to the point where it hurts usability.\
Unlike Haskell, unsafe IO dependent functions are used the same way as any other function. \
\
Any function who's unsafe code fails will return the Result<> enum to be handled by any of the unwrap functions. If a functions return argument is the built-in Result<> enum. The function body will implicitly return early if an Err variant is encountered anywhere in the body, to be handled at an upper level with unwrap methods such as:
```rust
fn unwrap_or r fallback (result<T> T -> T)
    case r of
      Ok  v: v
      Err _: fallback
```

As an example of the implicit result returns, take a look at the example below:

```rust
fn sum_as_num strings ([string] -> result<int>)
    sum << map strings << \s: toInt s

fn main
    puts << into
         << unwrap_or 0
         << sum_as_num ["5", "5", "5"]
```
toInt is unsafe here since a string cannot always be converted to int. So if toInt fails the function will fully implicitly return the Err(E) variant of Result. But if nothing fails the resulting `int` returned by `sum` is implicitly converted to the `Ok(int)` variant of Result

### Rethinking Function Parameters
While currying can shorten code greatly, the functional styling of leaf (and Haskel) already is short by nature. The problem with currying is that typo mistakes and human errors no longer counts as parsing errors, instead your value accidentally turns into some curried function it isn't supposed to be.
Current plan is to instead of implicitly making everything curried, give the user the option to explicitly turn a lambda or a function into a "curried" one upon usage. 
```haskell
fn func x y (a b -> c)

fn example f ((b -> c) -> c)
    f

fn main
    || becomes (b -> c)
    #(func 0) 
    || becomes (b -> c)
    #(\y: _)
```

## Examples

Simply run them with `./leaf <leaf-file>`. \
If you don't want to install the leaf standard library to it's expected path you can use set the LEAF_PATH environment variable, making the fully portable command `LEAFPATH=leafstd/ cargo run --release examples/<leaf-file>` \
Remember to compile Leaf using the `--release` flag! `cargo build --release` otherwise you'll get a whole lot of debug output and greatly degraded performance. 

## Status

The project is not yet in an usable state but we're getting there! 

 - [x] Index declarations for ease of access
 - [x] Design and implement the basic runner
 - [x] Implement int/string
 - [x] Create actually runnable Leaf programs
 - [ ] Add a TON of assertions to improve error messages
 - [x] Implement all other primitive types (including lists)
 - [ ] Design and implement list mutation
 - [x] Build a (Rust -> Leaf) bridge
 - [ ] Write a low-level standard library in Rust using the bridge
 - [ ] Implement conversions between the primitive types
 - [x] Add logic operations (if, elif, else)
 - [x] Swap out string identifier to vec indexes at parse-time for huge performance boosts
 - [ ] Add logic operations (match)
 - [ ] Add lambda support
 - [ ] Implement custom types (structs/enums)
 - [ ] Implement Leaf conversions
 - [ ] Design and implement generics
 - [ ] Internally design and implement implicit result handling
 - [ ] Write Leaf's standard library abstracting over the Rust bridge
 - [ ] Add a proper CLI
 - [ ] Design and Implement multithreadding (green-threadded concurrent mapping is an idea)
 - [x] Implement stack-safe recursion

