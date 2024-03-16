use std::fmt::Write;

use crate::parse::*;
// Place holder variable/generic
type TypeVar = u64;
type TypeIdx = usize;
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Base {
    Fun,
    Sum,
    Float,
    Int,
    Unit,
    Bool,
}
#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Closed(Box<Type>),
    Var(TypeVar),
    // really should split this into function and base but the reference said to treat stuff like int as just zero param
    // type functions and that did make sense and save space when i had multiple type functions (function and sum)
    // but now no sum but anyways
    Base(Base, Vec<Type>),
    // Ref type to avoid cloning too much
    Ref(TypeIdx),
    // mu type, µx.<type> just means that every x inside the <type> is the type of the whole mu expression
    Mu(Box<Type>),
    // but we use de Brujin indices instead of letter args
    MuRef(u64),
}
use Type::*;
impl Type {
    pub fn dbg(&self, out: &mut String, ctx: &Context, right: bool) {
        match self {
            Closed(t) => {
                write!(out, "Closed ");
                t.dbg(out, ctx, false);
            }
            Var(v) => {
                write!(out, "'{}", v);
            }
            Base(b, ts) => match b {
                Base::Fun => {
                    if !right {
                        write!(out, "(");
                    };
                    ts[0].dbg(out, ctx, false);
                    write!(out, " → ");
                    ts[1].dbg(out, ctx, true);
                    if !right {
                        write!(out, ")");
                    }
                }
                Base::Sum => todo!(),
                Base::Float => {
                    write!(out, "float");
                }
                Base::Int => {
                    write!(out, "int");
                }
                Base::Unit => {
                    write!(out, "unit");
                }
                Base::Bool => {
                    write!(out, "bool");
                }
            },
            Mu(t) => {
                write!(out, "µ(");
                t.dbg(out, ctx, true);
                write!(out, ")");
            }
            MuRef(i) => {
                write!(out, "<{}>", i);
            }
            Ref(idx) => {
                let t = ctx.types[*idx].as_ref();
                if let Some(t) = t {
                    t.dbg(out, ctx, right);
                } else {
                    write!(out, "nullref");
                }
            }
            _ => {}
        }
    }
    pub fn equal(&self, other: &Type, ctx: &[Option<Type>]) -> bool {
        match (self, other) {
            (Ref(a), b) => {
                if let Some(a) = &ctx[*a] {
                    a.equal(b, ctx)
                } else {
                    false
                }
            }
            (a, Ref(b)) => Ref(*b).equal(a, ctx),
            (Closed(a), b) => a.equal(b, ctx),
            (a, Closed(b)) => a.equal(b, ctx),
            (Type::Base(a, ats), Type::Base(b, bts)) => {
                a == b && ats.iter().zip(bts.iter()).all(|(a, b)| a.equal(b, ctx))
            }
            (Mu(a), Mu(b)) => a.equal(b, ctx),
            (a @ Mu(_), b) => unroll(a.clone()).equal(b, ctx),
            (a, b @ Mu(_)) => b.equal(a, ctx),
            _ => self == other,
        }
    }
    /// Check if a type contains a type variable
    fn contains(&self, ctx: &Context, var: u64) -> bool {
        match self {
            Var(v) => *v == var,
            Base(_, args) => args.iter().any(|t| t.contains(ctx, var)),
            Closed(t) => t.contains(ctx, var),
            Ref(id) => ctx.types[*id]
                .as_ref()
                .map(|t| t.contains(ctx, var))
                .unwrap_or(false),
            _ => false,
        }
    }
    /// Apply a substitution to a type
    fn apply(&mut self, sub: &Sub) {
        if self == &sub.target {
            dbg!(&sub);
            *self = sub.v.clone();
            return;
        }
        match self {
            Var(v) => {}
            Base(_, args) => {
                for a in args.iter_mut() {
                    a.apply(sub)
                }
            }
            Closed(t) => t.apply(sub),
            Mu(t) => t.apply(sub),
            Ref(_) | MuRef(_) => {} // Later do tree descent but for now linear covers everything
        }
    }
    fn apply_all(&mut self, subs: &[Sub]) -> bool {
        for sub in subs.iter().rev() {
            self.apply(sub);
        }
        false
    }
}
/// A substitution is an instruction to replace all instances of <target> with <v>
#[derive(PartialEq, Debug, Clone)]
struct Sub {
    target: Type,
    v: Type,
}
fn sub(v: Type, target: Type) -> Sub {
    Sub { v, target }
}
/// Calculates which substitutions are needed to make two types equal.
/// eg if expression foo must have type 'a -> int and float -> 'b, then substitute float
/// for 'a and int for 'b to get float -> int
/// the main motivation for substitutions is that it means you can apply the new information
/// globally very easily
fn unify(ctx: &Context, a: Type, b: Type) -> Option<Vec<Sub>> {
    Some(match (a, b) {
        // Equal, no subs needed
        (a, b) if a.equal(&b, &ctx.types) => {
            vec![]
        }
        // Merge the two variables
        (Var(a), Var(b)) if a != b => {
            vec![sub(Var(a), Var(b))]
        }
        // Replace the variable with t, if t doesn't contain the variable
        (Var(a), t) if !t.contains(ctx, a) => {
            vec![sub(t, Var(a))]
        }
        (t, Var(b)) if !t.contains(ctx, b) => {
            vec![sub(t, Var(b))]
        }
        // Now its known that t does contain the variable, so its recursive and we use mu
        (Var(a), mut t) => {
            dbg!(a, &t);
            replace_in(a, &mut t, 0);
            vec![sub(Mu(Box::new(t)), Var(a))]
        }
        (t, Var(b)) => unify(ctx, Var(b), t)?,
        // For functions, unify the last pair of args, apply all subsitutions, and then unify the second
        (Base(a, mut l1), Base(b, mut l2)) if l1.len() == l2.len() => {
            let u_1 = unify(ctx, l1.pop()?, l2.pop()?)?;
            let mut ap = Base(a, l1);
            let mut bp = Base(b, l2);
            ap.apply_all(&u_1);
            bp.apply_all(&u_1);
            let mut u = unify(ctx, ap, bp)?;
            u.extend_from_slice(&u_1);
            u
        }
        _ => return None,
    })
}
// Replace instances of v in target with the appropriate index for the mu type
fn replace_in(v: TypeVar, target: &mut Type, depth: u64) {
    if target == &Var(v) {
        *target = MuRef(depth);
        return;
    }
    match target {
        Type::Base(_, ts) => {
            for t in ts.iter_mut() {
                replace_in(v, t, depth);
            }
        }
        Mu(t) => replace_in(v, t, depth + 1),
        _ => {}
    }
}
fn unroll_step(v: &Type, target: &mut Type, depth: u64) {
    match target {
        Type::Base(_, ts) => {
            for t in ts.iter_mut() {
                unroll_step(v, t, depth);
            }
        }
        Mu(t) => unroll_step(v, t, depth + 1),
        MuRef(i) => {
            if *i == depth {
                *target = v.clone()
            }
        }
        _ => {}
    }
}
/// Unrolls a mu, so that if it's a function it can be called, eg µx. int -> x becomes int -> µx. int -> x
pub fn unroll(v: Type) -> Type {
    let cloned = v.clone();
    match v {
        Mu(mut t) => {
            unroll_step(&cloned, &mut t, 0);
            unroll(*t)
        }
        t => t,
    }
}
#[derive(Debug)]
pub struct Context {
    // Types of all expressions, Nones haven't been evaluated yet
    pub types: Vec<Option<Type>>,
    // for generating a new type var
    pub next_free: TypeVar,
}
impl Context {
    fn apply(&mut self, subs: &[Sub]) {
        for a in self.types.iter_mut().flatten() {
            a.apply_all(subs);
        }
    }
    fn gen(&mut self) -> TypeVar {
        self.next_free += 1;
        self.next_free - 1
    }
    // Clone a type and follow Refs
    fn full_clone(&self, t: Type) -> Option<Type> {
        match t {
            Closed(t) => Some(Closed(Box::new(self.full_clone(*t)?))),
            Base(b, ts) => {
                let mut v = Vec::with_capacity(ts.len());
                for t in ts {
                    v.push(self.full_clone(t)?);
                }
                Some(Base(b, v))
            }
            Ref(idx) => self.full_clone(self.types[idx].clone()?),
            t => Some(t),
        }
    }
    // Instance a polymorphic function
    fn instance(&mut self, t: Type) -> Option<Type> {
        match t {
            Closed(t) => {
                let mut t = self.full_clone(*t)?;
                let initial = self.gen();
                while let Some(sub) = self.instance_step(&t, initial) {
                    t.apply(&sub);
                }
                Some(t)
            }
            _ => Some(t),
        }
    }
    fn instance_step(&mut self, t: &Type, initial: TypeVar) -> Option<Sub> {
        match t {
            Base(_, ts) => ts
                .iter()
                .map(|t| self.instance_step(t, initial))
                .flatten()
                .next(),
            Var(v) => {
                if *v < initial {
                    Some(sub(Var(self.gen()), Var(*v)))
                } else {
                    None
                }
            }
            Mu(t) => self.instance_step(t, initial),
            MuRef(_) => None,
            _ => unreachable!(),
        }
    }
    pub fn dbg(&mut self, exprs: &[Expr], src: &str) -> Vec<String> {
        let mut ts = Vec::with_capacity(exprs.len());
        for idx in 0..exprs.len() {
            let r = &exprs[idx].range;
            print!("{}: ", src[r.clone()].trim());
            let mut s = String::new();
            self.types[idx].as_ref().map(|v| v.dbg(&mut s, self, true));
            println!("{}", &s);
            ts.push(s);
        }
        ts
    }
}
pub fn infer(ctx: &mut Context, exprs: &[Expr], idx: ExprIdx) -> Option<()> {
    match &exprs[idx].ty {
        ExprType::Abs {
            arg,
            ty,
            strict,
            body,
        } => {
            // Create new variable for the argument type
            let b = ctx.gen();
            ctx.types[*arg] = Some(Var(b));
            // idx is the current expression, assign it b -> type of body
            ctx.types[idx] = Some(Base(Base::Fun, vec![Ref(*arg), Ref(*body)]));
            infer(ctx, exprs, *body);
        }
        ExprType::App(e1, e2) => {
            infer(ctx, exprs, *e1);
            infer(ctx, exprs, *e2);
            // Create new variable for the output type
            let b = ctx.gen();
            let alt = Base(Base::Fun, vec![Ref(*e2), Var(b)]);
            ctx.types[idx] = Some(Var(b));
            // Make sure every branch sets ctx.types[idx] to Some when correct or else this fails
            let ty = ctx.types[*e1].clone()?;
            // We have two types for e1, the type inferred from its actual expression, and the fact that it has to
            // be a function that takes the type of e2 and returns b, this unifies the two types
            ctx.apply(&unify(
                ctx,
                ctx.full_clone(alt)?,
                unroll(ctx.full_clone(ty)?),
            )?);
        }
        ExprType::Let {
            definition,
            var,
            val,
            cont,
        } => {
            // Variable for the new variable
            ctx.types[*var] = Some(Var(ctx.gen()));
            infer(ctx, exprs, *val);
            let var_ty = unroll(ctx.full_clone(ctx.types[*var].clone()?)?);
            let val_ty = unroll(ctx.full_clone(ctx.types[*val].clone()?)?);
            // Unify the type of the variable and the type of the value assigned to it
            ctx.apply(&unify(ctx, var_ty, val_ty)?);
            if *definition {
                // If polymorphic, we close the type, so that future uses of it don't change it
                // since it gets instanced with new variables instead of referred to directly
                ctx.types[*var] = Some(Closed(Box::new(ctx.types[*var].clone()?)))
            }
            infer(ctx, exprs, *cont);
            ctx.types[idx] = Some(Ref(*cont));
        }
        ExprType::Var(_) => {}
        ExprType::Ident(_, var) => ctx.types[idx] = ctx.instance(ctx.types[*var].clone()?),
        ExprType::Lit(l) => match l {
            Literal::Float(_) => ctx.types[idx] = Some(Base(Base::Float, vec![])),
            Literal::Int(_) => ctx.types[idx] = Some(Base(Base::Int, vec![])),
        },
    }
    Some(())
}
