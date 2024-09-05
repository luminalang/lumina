# Types


**Integer Types**

| Size    | Unsigned | Signed |
|:--------|:---------|:-------|
| 8 bits  | u8       | i8     |
| 16 bits | u16      | i16    |
| 32 bits | u32      | i32    |
| 64 bits | u64      | i64    |
| Arch    | uint     | int    |

<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; `Arch` refers to that the size depends on the CPU architecture</sub></sup>

**Decimal Types**

`float` is supported as a double-precision 64-bit floating point. 

**Bools**

The `bool` type has either the value `true` or `false`

**Arrays**

Arrays are planned but currently not exposed to the user. 

**Prelude**

The following types aren't builtins but are available in prelude by default. 

```lm
type string // utf-8 encoded bytes
type List a // (or [a]) an efficient dynamic list
type Maybe a
type Result a err
type nothing
trait Num
trait Compare
trait ToString
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; `string` is incomplete and not yet validated as utf-8</sub></sup>

## Record types

Record types are types that contain other types as named fields. 

Sometimes also called `struct` or `product` types. 

```lm
type User {
  name string
  age  int
}

// Construct a new record of type `User`
fn main =
  { User | name = "Jonas", age = 25 }

// Construct a new record of type `User`
// 
// this time the type is inferred from the field names in scope
// instead of explicitly annotated. 
fn main =
  { name = "Jonas", age = 25 }
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; Defining and constructing a record</sub></sup>

**Modifying Records**

Records can also be constructed by changing some fields of an existing record. 

But remember! Lumina is an immutable language, so this creates a new record while keeping the previous record the same. 

```lm
fn rename user new as User, string -> User =
  { user ~ name = new }

// Is equivalent to: 

fn rename user new as User, string -> User =
  { User | name = new, age = user.age }
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; Function returning a copy of a user with its name substituted</sub></sup>

## Sum Types

Sum types are types whose value is one out of a set of possible variants. 

Sometimes called `enum` types. 

```lm
// Define a sum-type whose value can be one of `Admin`, `Client` or `Guest`
type UserKind = Admin | Client | Guest

// Construct a new sum-type of type `UserKind`
fn main = 
  Admin
```

Variants of sum types can also have parameters of other types.

```lm
type UserKind
  = Admin Department
  | Client Company
  | Guest

type Department = Accounting | Support

type Company {
  name string
}

fn main = 
  Admin Accounting

// or perhaps ...

fn main = 
  Client { Company | name = "luminalang" }
```

## Trait Types

*TODO: explain traits, and pretend dynamically dispatched objects are normal, we can explain trait constraints later.*

## Generic Type Parameters

<sup>*TODO: figure out how best to explain the practical use-cases and importance of generic type parameters.*</sup>

Declared types can take generic type parameters. 

```lm
type User item {
  name string
  age  int

  held item
}

fn main =
  // Here we supple the type `string` as type parameter for `User`
  // which replaces the `item` generic. 
  //
  // Meaning that the value assigned to the field `held` should be of type `string`.
  { User string | name = "Jonas", age = 25, held = "Laptop" }
```

```lm
// A type which is either "just" a value or "nothing". 
type Maybe value
  = Nothing
  | Just value

// Here we supply the type `int` as type parameter for `Maybe`
// which replaces the `value` generic.
//
// Meaning that the parameter to `Just` should be of type `int`. 
fn just_five as Maybe int = Just 5
```
<sup><sub>&nbsp;&nbsp;&nbsp;&nbsp; Two examples of defining a type with type parameters and then instantiating it. </sub></sup>

<sup><sub>*Since `Maybe` is known to be very useful, it's already defined in the Lumina standard library. </sub></sup>

