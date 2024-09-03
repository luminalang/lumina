# Pattern Matching & Conditionals

<sup>*TODO: Explain the purpose of patterns, their distinction from expressions, and how pattern matching is a core tool of functional programming.*</sup>

**Pattern Matching with `match`**

```lm
// Matching integers
fn check_integer n as int -> string =
  match n
  | 0    -> "zero"
  | 1    -> "one"
  | 2..9 -> "digit"
  | _    -> "large number"

// Matching sum types
fn or default m as int, Maybe int -> int =
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
