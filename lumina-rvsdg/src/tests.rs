use super::*;
use std::io::Write;

#[test]
fn phi() {
    lumina_util::test_logger();

    let mut unit = Node::create_translation_unit("testing");
    let region = &mut unit.downcast_mut::<nodes::TranslationUnit>().kind.region;

    let main = region.add_lambda_node("main");

    let mr = &mut region.get_mut(main).kind.region;

    let phi = mr.add_recenv_node();
    let fa = mr.add_output(phi, Meta::closure("fa"));
    let fb = mr.add_output(phi, Meta::closure("fb"));

    let mut phiref = mr.get_mut(phi);

    {
        let (lambda, _) = phiref.add_lambda_node("fa");
        let lref = phiref.get_mut(lambda);
        let x = lref.kind.region.add_argument(Meta::int("x"));
        lref.kind.region.add_argument(Meta::closure("fa"));
        lref.kind.region.add_argument(Meta::closure("fb"));
        let result = lref.kind.region.add_result(Meta::int("out"));

        let out = const_application(&mut lref.kind.region, 2, x, "-");

        // lref.kind.region.connect(fa);

        lref.kind.region.connect(out, result);
    }

    {
        let (lambda, _) = phiref.add_lambda_node("fb");
        let lref = phiref.get_mut(lambda);
        let y = lref.kind.region.add_argument(Meta::int("y"));
        lref.kind.region.add_argument(Meta::closure("fa"));
        lref.kind.region.add_argument(Meta::closure("fb"));
        let result = lref.kind.region.add_result(Meta::int("out"));

        let out = const_application(&mut lref.kind.region, 3, y, "+");
        lref.kind.region.connect(out, result);
    }

    export(&unit);
}

#[test]
fn phi_nodes_not_using_each_other() {
    lumina_util::test_logger();

    let mut unit = Node::create_translation_unit("testing");
    let region = &mut unit.downcast_mut::<nodes::TranslationUnit>().kind.region;

    let main = region.add_lambda_node("main");

    let mr = &mut region.get_mut(main).kind.region;

    let phi = mr.add_recenv_node();

    let mut phiref = mr.get_mut(phi);

    {
        let (lambda, _) = phiref.add_lambda_node("fa");
        let lref = phiref.get_mut(lambda);

        let one = num_node(&mut lref.kind.region, 1);
        let result = lref.kind.region.add_result(Meta::int("four"));

        lref.kind.region.connect(one, result);
    }

    {
        let (lambda, _) = phiref.add_lambda_node("fb");
        let lref = phiref.get_mut(lambda);

        let one = num_node(&mut lref.kind.region, 1);
        let result = lref.kind.region.add_result(Meta::int("five"));

        lref.kind.region.connect(one, result);
    }

    export(&unit);
}

#[test]
fn phi_nodes_indirect_recursion() {
    lumina_util::test_logger();

    let mut unit = Node::create_translation_unit("testing");
    let unit_region = &mut unit.downcast_mut::<nodes::TranslationUnit>().kind.region;

    let main = unit_region.add_lambda_node("main");
    let main_output = unit_region.add_output(main, Meta::closure("main"));
    unit_region.connect(main_output);

    let mr = &mut unit_region.get_mut(main).kind.region;

    let phi = mr.add_recenv_node();

    let mut phiref = mr.get_mut(phi);
    let (fa, fa_output) = phiref.add_lambda_node("fa");
    let (fb, _fb_output) = phiref.add_lambda_node("fb");

    let fa_x_param = phiref.add_parameter_to_lambda(fa, Meta::int("x"));
    let fb_y_param = phiref.add_parameter_to_lambda(fb, Meta::int("y"));

    {
        let closure = phiref.get_phi_closure(fa, fb);
        let lref = phiref.get_mut(fa);

        let num = const_application(&mut lref.kind.region, 1, fa_x_param, "+");

        // Call the other function with the incremented/decremented value
        let applied = {
            let apply = lref.kind.region.add_apply_node();
            let input_closure = lref.kind.region.add_input(apply, Meta::closure("fb"));
            let input_param = lref.kind.region.add_input(apply, Meta::int("num"));
            let output = lref.kind.region.add_output(apply, Meta::int("num"));

            lref.kind.region.connect(closure, input_closure);
            lref.kind.region.connect(num, input_param);

            output
        };

        // Connect the yield of the function application as a result
        let result = lref.kind.region.add_result(Meta::int("num"));
        lref.kind.region.connect(applied, result);
    }

    {
        let closure = phiref.get_phi_closure(fb, fa);
        let lref = phiref.get_mut(fb);

        let num = const_application(&mut lref.kind.region, 1, fb_y_param, "-");

        // Call the other function with the incremented/decremented value
        let applied = {
            let apply = lref.kind.region.add_apply_node();
            let input_closure = lref.kind.region.add_input(apply, Meta::closure("fa"));
            let input_param = lref.kind.region.add_input(apply, Meta::int("num"));
            let output = lref.kind.region.add_output(apply, Meta::int("num"));

            lref.kind.region.connect(closure, input_closure);
            lref.kind.region.connect(num, input_param);

            output
        };

        // Connect the yield of the function application as a result
        let result = lref.kind.region.add_result(Meta::int("num"));
        lref.kind.region.connect(applied, result);
    }

    let unit_region = &mut unit.downcast_mut::<nodes::TranslationUnit>().kind.region;
    let fa_entry = unit_region.add_input(main, Meta::closure("fa"));
    unit_region.connect(Origin::Output(phi.id, fa_output), fa_entry);

    // let main_fa_input = region.add_input(main, Meta::closure("fa"));
    // region.connect(Origin::output(phi.id, 0), main_fa_input);

    export(&unit);
}

fn num_node(r: &mut Region, n: usize) -> Origin {
    let num = r.add_const_node(n);
    r.add_output(num, Meta::int("num"))
}

fn const_application(r: &mut Region, x: usize, y: Origin, op: &'static str) -> Origin {
    let num_output = num_node(r, x);

    let action = r.add_builtin_node(op);
    let x_arg = r.add_input(action, Meta::int("x"));
    let y_arg = r.add_input(action, Meta::int("y"));
    let final_ = r.add_output(action, Meta::int("action result"));

    r.connect(num_output, x_arg);
    r.connect(y, y_arg);

    final_
}

#[test]
fn simpler() {
    lumina_util::test_logger();

    let mut unit = Node::create_translation_unit("testing");
    let region = &mut unit.downcast_mut::<nodes::TranslationUnit>().kind.region;

    let main = region.add_lambda_node("main");

    let mr = &mut region.get_mut(main).kind.region;
    let result = mr.add_result(Meta::int("out"));

    let one = mr.add_const_node(1);
    let two = mr.add_const_node(2);
    let plus = mr.add_builtin_node("+");

    let one_out = mr.add_output(one, Meta::int("one"));
    let two_out = mr.add_output(two, Meta::int("two"));

    let x_in = mr.add_input(plus, Meta::int("x"));
    let y_in = mr.add_input(plus, Meta::int("y"));

    let out = mr.add_output(plus, Meta::int("out"));

    mr.connect(out, result);
    mr.connect(one_out, x_in);
    mr.connect(two_out, y_in);

    export(&unit);
}

fn export(root: &Node) {
    let xml = xml::to_xml(&root);
    let path = "/home/simon/lumina-tests.rvsdg";
    let mut f = std::fs::File::create(path).unwrap();
    write!(f, "{}", xml).unwrap();
    println!(" wrote to {path}");

    std::process::Command::new("rvsdg-viewer")
        .arg(path)
        .spawn()
        .unwrap();
}
