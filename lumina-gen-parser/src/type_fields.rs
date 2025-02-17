use super::*;

impl<'s> Entity<'s> {
    pub fn iter_as_type_decl_fields<'a>(
        src: &'s str,
        seq: &'a [Tr<Entity<'s>>],
    ) -> FieldsIter<'a, 's> {
        FieldsIter { seq, src, i: 0 }
    }
}

pub struct FieldsIter<'a, 's> {
    seq: &'a [Tr<Entity<'s>>],
    src: &'s str,
    i: usize,
}

impl<'a, 's> Iterator for FieldsIter<'a, 's> {
    type Item = Vec<Tr<&'a Entity<'s>>>;

    fn next(&mut self) -> Option<Self::Item> {
        let fieldname = self.seq.get(self.i)?;
        self.i += 1;

        match &fieldname.value {
            Entity::Symbol(",") => self.next(),
            _ => {
                let mut members = vec![fieldname.as_ref()];

                // Iterate until comma or newline
                let mut end = fieldname.span.end().indice;
                while self.part_of_this_field(&mut end) {
                    let elem = self.seq[self.i].as_ref();
                    if matches!(elem.value, Entity::Symbol(name) if *name == ",") {
                        self.i += 1;
                        break;
                    }
                    members.push(elem);
                    self.i += 1;
                }

                Some(members)
            }
        }
    }
}

impl<'a, 's> FieldsIter<'a, 's> {
    fn part_of_this_field(&mut self, end: &mut u32) -> bool {
        let Some(elem) = self.seq.get(self.i) else {
            return false;
        };

        if matches!(elem.value, Entity::Symbol(name) if name == ",") {
            return false;
        }

        let between = Span::new(*end, (elem.indice() - *end) as u16);
        *end = elem.span.end().indice;
        if between.get_str(self.src).contains('\n') {
            false
        } else {
            true
        }
    }
}
