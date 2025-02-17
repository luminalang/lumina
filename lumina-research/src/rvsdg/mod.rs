use lumina_key::Map;

mod ssa;
use ssa::*;
mod key;

pub fn run() {
    let mut ssa = SSA::new();

    let entry = KBlock(0);

    let adder = {
        let mut adder = Func::new();
        let p0 = adder.param(entry);
        let p1 = adder.param(entry);
        let r = adder.add(entry, p0, p1);
        adder.ret(entry, r);
        ssa.funcs.push(adder)
    };

    let main = {
        let mut main = Func::new();
        let _2 = main.uconst(entry, 2);
        let _4 = main.uconst(entry, 4);
        let r = main.call(entry, adder, [_2, _4]);
        main.ret(entry, r);
        ssa.funcs.push(main)
    };

    build_rvsdg(&ssa);
}

fn build_rvsdg(ssa: &SSA) -> Region {
    todo!();
}

enum ValueOrState {
    Value(Value),
    State(State),
}

struct State;
struct Value(u32);

struct Region {
    arguments: Map<key::Argument, Vec<Edge>>,
    nodes: Map<key::Node, Node>,
    edges: Vec<Edge>,
    results: Map<key::Result, Edge>,
}

// EITHER: we remove `key::Node` from `Argument` so that there's no ambiguety
// OR:     we have `key::Node` on both and store all edges in the region.
//
// IF: we put all edges in the region. Then; we do need a way to distinguish region argument from
// node argument?

struct Edge {
    origin: (key::Node, Origin),
    user: (key::Node, User),
}

enum Origin {
    Argument(key::Argument),
    Output(key::Output),
}

enum User {
    Input(key::Input),
    Result(key::Result),
}

const G: &str = "origin";
const U: &str = "user";

struct Node {
    // inputs: Map<key::Input, Type>,
    // outputs: Map<key::Input, Type>,
    edges: Vec<Edge>,
    kind: NodeKind,
}

enum NodeKind {
    Simple(SimpleKind),
    Structural(),
}

// enum Node {
//     Simple {
//         inputs: Map<key::Input, Edge>,
//         outputs: Map<key::Output, Vec<Edge>>,
//         kind: SimpleKind,
//     },
//     Structural {
//         inputs: Map<key::Input, Edge>,
//         outputs: Map<key::Output, Vec<Edge>>,
//     },
// }

enum SimpleKind {
    Add,
    UConst(usize),
}

// fn uconst(n: usize, output: Edge) -> Node {
//     Node {
//         kind: NodeKind::Simple(SimpleKind::UConst(n)),
//         // Should edges be stored in region?
//         // That probably would make analysis a lot harder
//         edges: vec![Edge {
//             origin: (key::Node(self), Origin::Output(key::Output)),
//             user: (key::Node()),
//         }],
//     }
//     // Node::Simple {
//     //     inputs: [].into(),
//     //     outputs: [vec![output]].into(),
//     //     kind: SimpleKind::UConst,
//     // }
// }
//
// fn add(x: Edge, y: Edge, output: Edge) -> Node {
//     Node::Simple {
//         inputs: [x, y].into(),
//         outputs: [vec![output]].into(),
//         kind: SimpleKind::Add,
//     }
// }

// THINGS TO TRY:
//
// Instead of directly putting edges in outputs/inputs:
// we could instead just have a `Vec<Edge>`? Might make this work
//
// Constructing depth-first consumer-first is probably the intended way.

fn add_two_constants() {
    todo!();
}
