mod infer;
mod parse;
mod report;
mod typecheck;
use crate::{infer::*, typecheck::check};
use std::time::Instant;
fn fun(a: Type, b: Type) -> Type {
    Type::Base(Base::Fun, vec![a, b])
}
fn main() {
    let src = include_str!("../test.yk");
    let mut exprs = Vec::new();
    let scope = crate::parse::Scope {
        prev: None,
        var: "".into(),
        id: 0,
    };
    // Add some words to the start scope
    let scope = scope.keyword("float", &mut exprs);
    let scope = scope.keyword("int", &mut exprs);
    let scope = scope.keyword("unit", &mut exprs);
    let scope = scope.keyword("true", &mut exprs);
    let scope = scope.keyword("false", &mut exprs);
    let scope = scope.keyword("if", &mut exprs);
    let e = dbg!(crate::parse::parse(
        crate::parse::ParseState {
            s: src.trim_start(),
            start: 0,
        },
        scope,
        &mut exprs,
    ));
    let mut ctx = Context {
        types: vec![None; exprs.len()],
        next_free: 1,
    };
    // give those words types (this is dumb but like its fine ill make start up pretty later)
    ctx.types[0] = Some(Type::Base(Base::Float, vec![]));
    ctx.types[1] = Some(Type::Base(Base::Int, vec![]));
    ctx.types[2] = Some(Type::Base(Base::Unit, vec![]));
    ctx.types[3] = Some(Type::Base(Base::Bool, vec![]));
    ctx.types[4] = Some(Type::Base(Base::Bool, vec![]));
    ctx.types[5] = Some(Type::Closed(Box::new(fun(
        Type::Base(Base::Bool, vec![]),
        fun(
            fun(Type::Base(Base::Unit, vec![]), Type::Var(0)),
            fun(
                fun(Type::Base(Base::Unit, vec![]), Type::Var(0)),
                Type::Var(0),
            ),
        ),
    ))));
    let start = Instant::now();
    infer(&mut ctx, &exprs, exprs.len() - 1);
    let names = ctx.dbg(&exprs, src);
    let errors = check(&ctx.types, &exprs, &names);
    for error in errors {
        error.report(src);
    }
    dbg!(Instant::now() - start);
}
