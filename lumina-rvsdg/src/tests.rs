use super::*;

#[test]
fn testing() {
    let mut unit = Node::create_translation_unit();
    let region = &mut unit.downcast_mut::<nodes::TranslationUnit>().kind.region;

    let main = region.add_lambda_node("main");
    region.connect_to_new_result(Origin::output(main, 0));

    let recenv = {
        let recenv = region.add_recenv_node();
        let mut recenvref = region.get_mut::<nodes::RecEnv>(recenv);

        let fa_node_id = recenvref.add_lambda_node("fa");
        let fb_node_id = recenvref.add_lambda_node("fb");

        let fa_x = recenvref.add_parameter_to_lambda(fa_node_id, Meta::int("x"));
        let fb_y = recenvref.add_parameter_to_lambda(fb_node_id, Meta::int("y"));

        {
            let faref = recenvref.get_mut(fa_node_id);
            let faregion = &mut faref.kind.region;

            let plus = {
                let one = faregion.add_simple_node(nodes::Placeholder("1"));
                let plus = faregion.add_simple_node(nodes::Placeholder("+"));
                faregion.connect(Origin::output(one, 0), User::input(plus, 0));
                faregion.connect(Origin::Argument(fa_x), User::input(plus, 1));
                plus
            };

            let apply = faregion.add_simple_node(nodes::Apply::new());

            // TODO: What's the pretty way to do this
            faregion.connect(Origin::arg(2), User::input(apply, 0));
            faregion.connect(Origin::output(plus, 0), User::input(apply, 1));
        }

        {
            let fbref = recenvref.get_mut(fb_node_id);
            let fbregion = &mut fbref.kind.region;

            let plus = {
                let one = fbregion.add_simple_node(nodes::Placeholder("1"));
                let plus = fbregion.add_simple_node(nodes::Placeholder("-"));
                fbregion.connect(Origin::output(one, 0), User::input(plus, 0));
                fbregion.connect(Origin::Argument(fb_y), User::input(plus, 1));
                plus
            };

            let apply = fbregion.add_simple_node(nodes::Apply::new());

            // TODO: What's the pretty way to do this
            fbregion.connect(Origin::arg(2), User::input(apply, 0));
            fbregion.connect(Origin::output(plus, 0), User::input(apply, 1));
        }

        recenv
    };

    // Connect `fa` to `main`
    region.connect(Origin::output(recenv, 0), User::input(main, 0));
    {
        let mainref = region.get_mut::<nodes::Lambda>(main);
        let mainregion = &mut mainref.kind.region;

        let five = mainregion.add_simple_node(nodes::Placeholder("5"));
        let apply = mainregion.add_simple_node(nodes::Apply::new());

        mainregion.connect(Origin::output(recenv, 0), User::input(apply, 0));
        mainregion.connect(Origin::output(five, 0), User::input(apply, 1));
    }

    // let main = unitref.define_function_item(true, "main");

    todo!();
}
