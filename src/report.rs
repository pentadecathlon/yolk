use std::{ops::Range, rc::Rc};

pub struct Error {
    message: Rc<str>,
    range: Range<usize>,
}
pub fn report(source: &str) {}
