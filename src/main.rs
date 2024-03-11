mod infer;
mod parse;

use std::time::Instant;

use crate::infer::{infer, Context};
fn main() {
    let src = include_str!("../test.yk");
    let mut exprs = Vec::new();
    let e = dbg!(crate::parse::parse(
        crate::parse::ParseState {
            s: src.trim_start(),
            start: 0,
        },
        crate::parse::Scope {
            prev: None,
            var: "".into(),
            id: 0
        },
        &mut exprs
    ));
    let mut ctx = Context {
        types: vec![None; exprs.len()],
        next_free: 0,
    };
    let start = Instant::now();
    infer(&mut ctx, &exprs, exprs.len() - 1);
    ctx.dbg(dbg!(&exprs), src);
    dbg!(Instant::now() - start);
}
