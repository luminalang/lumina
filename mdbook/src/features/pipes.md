# Pipes (dot calls)

As functional programming often devolves to a tree of many nested function calls, Lumina provides numerous ways to split out nesting to make the code more readable. 

The builtin `pipe` construct serves as an alternative way of chaining function calls in a much more intuitive way. 

```lumina
forEach #io:println (filter #(> 6) (map #(+ 4) [1, 2, 3]))

// May instead be written as

[1, 2, 3]
  . map #(+ 4)
  . filter #(> 6)
  . forEach #io:println
```

However; `.` provides extra convenience by resolving functions from the module of where the type from the left-hand-side expression is defined. 

Instead of

```lumina
use std:list [forEach]
use std:io

fn main = [1, 2, 3] . forEach #io:println
```

You may write

```lumina
use std:io

// `[_]` is defined in `std:list`. Thus; `forEach` is resolved from the module `std:list` automatically. 
fn main = [1, 2, 3] . forEach #io:println
```
