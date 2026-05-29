# Pointer Arithmetics

Lumina has raw pointers for low-level code. For normal mutability, its advised to use `std:cell:Cell` instead.

```rs
use std:ptr [alloc, write, deref, offset, box]

let a = box 1 in
let b = alloc(t as u64) in
do write b 2 then
  deref b
```
