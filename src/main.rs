mod infer;
mod parse;

use std::time::Instant;
fn main() {
    let src = include_str!("../test.yk");

    let e = dbg!(crate::parse::parse(crate::parse::ParseState {
        s: src.trim_start(),
        start: 0
    }));
    let start = Instant::now();
    infer::test();
    dbg!(Instant::now() - start);
}
