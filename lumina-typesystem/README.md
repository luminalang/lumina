# Backtracked Micropass Type Inference

Experimental type checking and inference algorithm which splits inference into small passes ordered by user relevance, reruns earlier passes whenever new information appears, and attempts to emit errors which match the developers thought process as closely as possible.

The repository is meant to accompany [`this article`](https://articles.luminalang.com/a/micropass-inference/).

![Feature Complete Example](./img/feature-complete-example.rs.webp)

## Scope

This crate is not meant to be used as a library, but rather as the example of the idea which you may build on top on. 

It skips over key compiler concepts such as spans, name resolution, does very minimal error generation, and is written with a complete disregard for performance in favor of being more approachable. 

In this implementation the supported features are

- Rank-1 Parametric Polymorphism
- Type Level Parametric Polymorphism
- First Class Functions
- 'Same As' grouping where N values should all match in type
- Inference of product types by their fields
- Integer type placeholders


## Core Concept

Instead of one large unification pass, inference is split into ordered passes in [`src/inference_passes.rs`](src/inference_passes.rs):

1. `known_applications`
2. `known_assignments`
3. `known_return_types`
4. `known_same_as_unifications`
5. `known_record_fields`
6. `resolve_records`
7. `less_known_functions`
8. `default_numbers`
9. `default_unknown_to_unit_or_lift`

With this ordering the higher up the list you go the closer to the users thought process you are. This way types are inferred by what the user is likely to consider the most important rather than by their position in source code.

Each pass performs its minimal task leaving most types untouched. After a pass is completed, it re-runs all previous passes before it. Hoping the changes it made will facilitate earlier passes to perform their role. With this design it’s logical that the lower down the list you go the more aggressive passes are allowed to be with their inference. As its implied less new type information will be made available.

Since each type variable may be unified many times by the same pass, error generation is not reasonably able to work during inference. Therefore passes ignore errors and only touch the types it knows it can and should work with. Error generation is instead deligated to type checking.


## Example

See the [`integration tests`](tests/examples.rs) for more examples

```rust
fn generic_function_application() {
    let mut env = Environment::new();
    env.leave_signature_enter_expression();

    // let map = |list: [a], f: (a -> b)| -> [b]
    let func = {
        let a = || T::generic("a");
        let b = || T::generic("b");
        let f = T::function([a()], b());
        env.instantiate_function(["a", "b"], &[T::list(a()), f], &T::list(b()))
    };

    // let r = map([5_i32], |n: i32| n as i64);
    let ret = {
        let _i32 = env.i(32);
        let _i64 = env.i(64);
        let list = env.list(_i32);
        let f = env.function(vec![_i32], _i64);

        let application = env.apply(func);
        env.apply_next_parameter(application, list);
        env.apply_next_parameter(application, f);

        env.get_return_type(application)
    };

    // return r
    let expected_return_type = {
        let _i64 = env.i(64);
        env.list(_i64)
    };
    env.assign(ret, expected_return_type);

    InferenceUnifier::new(&mut env).infer();
    let assignments = Finalizer::new(&mut env).finalize_all();
    let errors = Checker::new(&assignments, &mut env).type_check();
}
```


## Concerns

While results are extremely promising, the concern with this approach is its innate quadratic complexity.

## Suggested Study Order

1. [`tests/snippets.rs`](tests/snippets.rs)
2. [`src/inference_passes.rs`](src/inference_passes.rs) (what makes this project unique)
3. [`src/lib.rs`](src/lib.rs) (type environment and constructors)
4. [`src/checker.rs`](src/checker.rs)

