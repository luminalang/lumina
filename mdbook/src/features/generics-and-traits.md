# Generics & Traits

Generics are used for making types and functions more flexible. 

Imagine a scenario such as

```lumina
fn select cond x y as bool string string -> string = 
  if cond
    then x
    else y

fn main =
  select (1 > 2) "this" "that"
  . io:println
```

Here we're using the type `string`. However; the function `select` doesn't actually care which type its used with. 

Instead; we use a generic type. 

```lumina
fn select cond x y as bool a a -> a =
  ...
```

When defining functions, any lowercase single-letter type will be treated as a generic type. 

Generics can also be used for type declarations to grant type parameters for a declared type. 

```lumina
type WithId v {
  id int 
  value v
}

fn create_user as string -> WithId User =
  ...
```


## Traits

If `type T {...}` defines what a type has, and `type T = A | B` defines what a type can have, then `trait` defines what a type should be able to do. 

```lumina
trait Animal
  fn make_noise as string
  fn move as (int, int) -> (int, int)
```

Traits can be used in one of two ways. Either as a type by itself, which we call a trait object. Or as a constraint for generic types. 

```lumina
// Used as trait object
fn add as Animal Zoo -> Zoo =
  ...

// Used as trait constraint
when
  a can Animal
fn add as a Zoo -> Zoo =
  ...
```

To implement a trait for a type, use the `impl` keyword. 

```lumina
type Cat

impl Animal for Cat
  fn make_noise = "meow"
  fn move (x, y) = (x + 5, y + 2)
```

<sup>*TODO: Most of this only makes sense if you're used to Rust or some degree Haskell. It should be explained more fundamentally.*</sup>
