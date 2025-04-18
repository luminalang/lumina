# Pattern Matching & Conditionals

**Pattern Matching with `match`**

Lumina provides pattern matching via the `match` keyword. A `match` expression takes one expression as input and then a set of branches which checks for a pattern to run a specified expression. 
A pattern is in some ways the inverse of an expression. Expressions construct and create data, while patterns destruct data. Destructing data with a pattern will allow you to check for whether the data looks a particular way, while also binding specific bits of data to new identifiers. 

Pattern matching is a core component of Lumina and will be the primary part of most functions. 

```lm
// Matching integers
fn check_integer n as int -> string =
  match n
  | 0    -> "zero"
  | 1    -> "one"
  | 2..9 -> "digit"
  | _    -> "large number"

// Matching sum types
fn or default m as int (Maybe int) -> int =
  match m
  | Nothing -> default
  | Just n  -> n

// Matching records
fn encode_user user as User -> string = 
  match user
  | { kind = Admin, name, .. } -> "admin_" <++> name
  | { kind = Guest, name, .. } -> "guest_" <++> name

  // Type annotation is optional
  | { User | kind = Guest, name, .. } -> "guest_" <++> name

// Matching strings
fn parse_user str as string -> (int, string) =
  match str
  | "id:" id " username:" username ->
    (to_digit id, username)
  | _ ->
    crash ("parsing error: " <++> str)

// Matching lists
fn first_three list as [int] -> Maybe (int, int, int) =
  match list
  | [x, y, z : _rest] -> Just (x, y, z)
  | _ -> Nothing

// Matching tuples
fn far_away position as (int, int) -> bool =
  match position
  | (100.., _) -> true
  | (_, 100..) -> true
  | (_, _)     -> false
```

<sup>*TODO: Should we show and explain string extractors here or under advanced features? Should probably be after partial application*</sup>

**If Expressions**

```lm
fn can_drive age =
  if age > 18
    then "This person can own a drivers license"
    else "This person can not own a drivers license"
```
