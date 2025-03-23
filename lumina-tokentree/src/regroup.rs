use super::*;

// I think the easiest is to just make `can` an identifier and then split_on for the entities

impl<'s> Entity<'s> {
    pub fn for_double_sided_operator<'a>(
        mut lhs: Tr<&'a Entity<'s>>,
        parts: &'a [Tr<Entity<'s>>],
        mut f: impl FnMut(Tr<&Entity<'s>>, &[Tr<Entity<'s>>]),
    ) {
        for (i, part) in parts.iter().enumerate() {
            match &part.value {
                Entity::Sequence(elems) => {
                    let (next_lhs, xs) = elems.split_last().unwrap();
                    f(lhs, xs);
                    lhs = next_lhs.as_ref();
                }
                _ => {
                    if i == parts.len() - 1 {
                        f(lhs, &[part.clone()])
                    } else {
                        f(lhs, &[Entity::Missing.tr(part.span)]);
                        lhs = part.as_ref();
                    }
                }
            }
        }
    }
}

// Let's try handling this in TT construction instead.
// impl<'s> Entity<'s> {
//     pub fn regroup_when_bindings(entity: Entity<'s>) -> Vec<Entity<'s>> {
//         match entity {
//             Entity::Sequence(elems) => {
//                 let mut elems = elems.into_iter();
//                 while let Some(lhs) = elems.next() {
//                     match elems.next() {
//                         Some(Tr {
//                             value: Entity::Header(Tr { value: "can", .. }, and_then),
//                             ..
//                         }) => {
//                             todo!()
//                         }
//                         _ => todo!(),
//                     }
//                     // let seq = vec![lhs];
//
//                     // while let Some(elem) = elems.next() {
//                     //     match &elem.value {
//                     //         Entity::Can
//                     //     }
//                     // }
//
//                     todo!();
//                 }
//
//                 todo!();
//             }
//             _ => vec![entity],
//         }
//     }
// }
