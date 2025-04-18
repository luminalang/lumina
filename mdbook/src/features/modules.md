# Module System

A Lumina project is structured like this

```
project-name
├── config.lm
└── src
    ├── main.lm
    ├── other_dir
    │   ├── file.lm
    │   └── lib.lm
    └── other_file.lm
```

When importing modules inside of Lumina source code, the relative filepath corresponds directly to the path in the use item. 

So from `main.lm` the other modules would be imported as. 

```lumina
use other_dir:file
use other_dir
use other_file
```

`lib.lm` and `main.lm` are magic filenames which are put under the namespace of the folder they're in. 
So to import `other_dir/lib.lm` in `main.lm` then instead of `use other_dir:lib`, you'd use `use other_dir`

When importing a module, you can also import items from that module directly. 

```lumina
use other_file:file [Direction [Right, Down]]
//                   ^ Item
//                              ^ Members under Item
```

As opposed to using items through the module name. 

```lumina
fn down = file:Direction:Down
```
