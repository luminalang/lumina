# Declaring Global Values

Global values can be declared with the `val` keyword. 

This is sometimes useful to provide clarity as opposed to defining functions for constants. 

```lumina
val min_members = 1
val max_members = 20
```

All initializations for global values occur before the `main` function is ran. 

If you're writing low-level code then it might be useful to receieve the raw pointer of a global value and mutate it at runtime.

```lumina
use std:io
use std:ptr

val count = 0

fn main =
  let ptr = builtin:val_to_ref count in
    do ptr:write ptr 5
     then io:println count
```
