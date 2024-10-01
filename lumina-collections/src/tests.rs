use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
struct TestKey(u32);

map_key_impl!(TestKey(u32), "test");

#[test]
fn keys_iter() {
    let map: Map<TestKey, usize> = Map::from([0, 1, 2, 3]);
    let vec: Vec<(TestKey, usize)> = map.keys().map(|k| (k, map[k])).collect();
    assert_eq!(
        vec.as_slice(),
        [0, 1, 2, 3].map(|i| (TestKey(i as u32), i)).as_slice()
    );

    let mut mmap: MMap<TestKey, usize> = MMap::from_iter([0, 1, 2, 3].map(Module::from));
    mmap.push_as(M(Module(0), TestKey(0)), 0);
    mmap.push_as(M(Module(1), TestKey(0)), 1);

    let secondary = mmap.secondary_with(|_, v| *v);
    assert_eq!(secondary, mmap);

    assert_eq!(
        KeysIter::up_to(TestKey(2)).collect::<Vec<TestKey>>(),
        vec![TestKey(0), TestKey(1)]
    );
}

#[test]
fn secondary() {
    let map: Map<TestKey, usize> = Map::from([0, 1, 2, 3]);

    let mut secondary = map.secondary();
    assert_eq!(secondary, Map::new());

    secondary.push_as(TestKey(0), 0);
    secondary.push_as(TestKey(1), 1);

    assert_eq!(secondary, Map::from([0, 1]));
}
