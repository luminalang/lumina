use super::*;

#[test]
fn example() {
    let int = || Value::Meta(Test::Int);

    Node::<Test>::omega(|region| {
        // val x = 1 + 2
        let constant = region.delta(&[], int(), |region| {
            let one = region.int(int(), 1);
            let two = region.int(int(), 2);
            let add = region.add(int());

            region.connect(Origin::output(one, 0), User::input(add, 0));
            region.connect(Origin::output(two, 0), User::input(add, 1));

            region.connect(Origin::output(add, 0), User::result(0));
        });

        // fn f0 v = f1 (v + 1)
        // fn f1 v = f0 (v - 1)
        let funs = region.phi(
            [
                RegionSignature::from(vec![int()], vec![int()]),
                RegionSignature::from(vec![int()], vec![int()]),
            ],
            |i, lambdas, region| {
                let one = Node::int(int(), 1);
                let one = region.nodes.push(one);

                let operation = match i {
                    0 => Node::add(int()),
                    _ => Node::sub(int()),
                };
                let operation = region.nodes.push(operation);

                region.connect(Origin::output(one, 0), User::input(operation, 0));
                region.connect(Origin::arg(0), User::input(operation, 1));

                // Call the other function
                let apply = region.apply(lambdas[0], &[Origin::output(operation, 0)]);

                region.connect(Origin::output(apply, 0), User::result(0));
            },
        );

        // fn main = f0 x
        let main = region.lambda(
            true,
            &[Origin::output(funs, 0), Origin::output(constant, 0)],
            &[],
            [int()],
            |region, _| {
                let f0 = Origin::arg(0);
                let constant = Origin::arg(1);
                let apply = region.apply(f0, &[constant]);
                region.connect(Origin::output(apply, 0), User::result(0))
            },
        );

        region.connect(Origin::output(main, 0), User::result(0));
    })
    .verify();
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Test {
    Int,
    Unit,
}
