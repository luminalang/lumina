# Updating Nested Records

```lumina
// Setting a field that's deeply nested in records
fn set_id id e as int Entity -> Entity =
  { e ~ information.tracking.item.id = id }

// Modifying a field that's deeply nested in records
fn inc_id e as Entity -> Entity =
  { e ~ information.tracking.item.id @ id = id + 1 }
```
