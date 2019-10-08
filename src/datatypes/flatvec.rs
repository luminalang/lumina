use std::fmt;

// A vec that expects its data to be [x, y, x, y, x, y, z]
// Used for things like If and Match statements

#[derive(Debug, Clone, PartialEq)]
pub struct FlatVec<T: fmt::Debug + Clone + Sized> {
    data: Vec<T>,
    sealed: bool,
}

impl<T: fmt::Debug + Clone + Sized> FlatVec<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::with_capacity(2),
            sealed: false,
        }
    }

    pub fn push_x(&mut self, v: T) {
        if (is_odd(self.data.len()) && self.data.is_empty()) || self.sealed {
            panic!("Poisoned IfStatement! {:?}", self);
        }
        self.data.push(v);
    }
    pub fn push_y(&mut self, v: T) {
        if (is_even(self.data.len()) && !self.data.is_empty()) || self.sealed {
            panic!("Poisoned IfStatement! {:?}", self);
        }
        self.data.push(v);
    }

    // Set the last and lock the data from future changes
    pub fn seal(&mut self, v: T) {
        if self.sealed {
            panic!("Already sealed");
        }
        self.data.push(v);
        self.sealed = true;
    }

    pub fn get_seal(&self) -> Option<&T> {
        if self.sealed {
            Some(self.data.last().unwrap())
        } else {
            None
        }
    }
    fn get_x_raw(&self, branch: usize) -> &T {
        if is_odd(branch) {
            panic!("call on {}", branch);
        }
        &self.data[branch]
    }
    fn get_y_raw(&self, branch: usize) -> &T {
        if is_even(branch) {
            panic!("call on {}", branch);
        }
        &self.data[branch]
    }
    pub fn get_x(&self, branch: usize) -> &T {
        self.get_x_raw(branch * 2)
    }

    pub fn get_y(&self, branch: usize) -> &T {
        self.get_y_raw((branch * 2) + 1)
    }
}

fn is_odd(i: usize) -> bool {
    i & 1 == 1
}
fn is_even(i: usize) -> bool {
    i & 1 == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn if_statement_allocation() {
        /*
        if 5
          then 10
          elif 15
            then 20
          elif 25
            then 30
          else 40
        */
        let mut stmt = FlatVec::new();
        stmt.push_x(5);
        stmt.push_y(10);
        stmt.push_x(15);
        stmt.push_y(20);
        stmt.push_x(25);
        stmt.push_y(30);
        stmt.seal(40);

        assert_eq!(
            stmt,
            FlatVec {
                sealed: true,
                data: vec![5, 10, 15, 20, 25, 30, 40],
            }
        );
        assert_eq!(*stmt.get_x(1), 15);
        assert_eq!(*stmt.get_y(1), 20);
        assert_eq!(*stmt.get_x(2), 25);
        assert_eq!(*stmt.get_y(2), 30);
        assert_eq!(stmt.get_seal().map(|n| *n), Some(40));
    }
}
