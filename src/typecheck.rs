use crate::{
    parse::Expr,
    report::{err, Error},
    Base, Type,
};
pub fn check(types: &[Option<Type>], exprs: &[Expr], names: &[String]) -> Vec<Error> {
    let mut errors = Vec::new();
    for (idx, expr) in exprs.iter().enumerate() {
        if types[idx].is_none() {
            errors.push(err("Could not infer this type", expr.range.clone()))
        }
        match expr.ty {
            crate::parse::ExprType::App(f, val) => match &types[f] {
                Some(Type::Base(Base::Fun, ts)) => {
                    if let Some(v) = &types[val] {
                        if !ts[1].equal(v, types) {
                            errors.push(err(
                                &format!(
                                    "Function type {} does not match input type {}",
                                    &names[f], &names[val]
                                ),
                                expr.range.clone(),
                            ))
                        }
                    }
                }
                None => {} // Gets caught later
                _ => errors.push(err("Expected a function", exprs[f].range.clone())),
            },
            crate::parse::ExprType::Let {
                definition,
                var,
                val,
                cont,
            } => {
                let Some(val_t) = &types[val] else { continue };
                let Some(var_t) = &types[var] else { continue };
                if !var_t.equal(val_t, types) {
                    errors.push(err(
                        &format!(
                            "Illegal recursion, {} and {} couldn't be unified",
                            names[var], names[val]
                        ),
                        exprs[val].range.clone(),
                    ))
                }
            }
            _ => {}
        }
    }
    errors
}
