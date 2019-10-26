use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default, Debug, Clone)]
pub struct DCE {
    used_operators: Rc<RefCell<HashMap<(usize, usize), ()>>>,
    used_functions: Rc<RefCell<HashMap<(usize, usize), ()>>>,
}

impl DCE {
    pub fn tag_func(&mut self, m: usize, f: usize) {
        self.used_functions.borrow_mut().insert((m, f), ());
    }
    pub fn tag_oper(&mut self, m: usize, f: usize) {
        self.used_operators.borrow_mut().insert((m, f), ());
    }

    pub fn used_functions(&self) -> Vec<(usize, usize)> {
        self.used_functions
            .borrow()
            .keys()
            .copied()
            .collect::<Vec<(usize, usize)>>()
    }
    pub fn used_operators(&self) -> Vec<(usize, usize)> {
        self.used_operators
            .borrow()
            .keys()
            .copied()
            .collect::<Vec<(usize, usize)>>()
    }
}
