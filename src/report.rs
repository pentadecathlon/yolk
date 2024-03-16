use std::{ops::Range, rc::Rc};

pub struct Error {
    message: Rc<str>,
    range: Range<usize>,
}
pub fn err(msg: &str, range: Range<usize>) -> Error {
    Error {
        message: msg.into(),
        range,
    }
}
impl Error {
    pub fn report(&self, source: &str) {
        let start = self
            .range
            .start
            .checked_sub(20)
            .unwrap_or(0)
            .clamp(0, source.len());
        let end = (self.range.start + 20).clamp(0, source.len());
        print!("{}", &source[start..self.range.start]);
        print!("\x1b[1;31m{}", &source[self.range.clone()]);
        print!("\x1b[0m{}\n", &source[self.range.end..end]);
        println!("{}", self.message)
    }
}
