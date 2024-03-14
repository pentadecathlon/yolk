use crate::parse::*;
type TypeVar = u64;
type Var = usize;
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
    Base(Base, Vec<Type>),
    Ref(TypeIdx),
}
use Type::*;
impl Type {
    fn dbg(&self, ctx: &Context, right: bool) {
        match self {
            Closed(t) => {
                print!("Closed ");
                t.dbg(ctx, false);
            }
            Var(v) => print!("'{}", v),
            Base(b, ts) => match b {
                Base::Fun => {
                    if !right {
                        print!("(")
                    };
                    ts[0].dbg(ctx, false);
                    print!(" → ");
                    ts[1].dbg(ctx, true);
                    if !right {
                        print!(")")
                    };
                }
                Base::Sum => todo!(),
                Base::Float => print!("float"),
                Base::Int => print!("int"),
                Base::Unit => print!("unit"),
                Base::Bool => print!("bool"),
            },
            Ref(idx) => ctx.types[*idx].as_ref().unwrap().dbg(ctx, right),
            _ => {}
        }
    }
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
    fn apply(&mut self, sub: &Sub) {
        match self {
            Var(v) => {
                if *v == sub.target {
                    *self = sub.v.clone();
                }
            }
            Base(_, args) => {
                for a in args.iter_mut() {
                    a.apply(sub)
                }
            }
            Closed(t) => t.apply(sub),
            Ref(_) => {} // Later do tree descent but for now linear covers everything
        }
    }
    fn apply_all(&mut self, subs: &[Sub]) -> bool {
        for sub in subs.iter().rev() {
            self.apply(sub);
        }
        false
    }
}
#[derive(PartialEq, Debug, Clone)]
struct Sub {
    target: u64,
    v: Type,
}
pub fn sub(v: Type, target: u64) -> Sub {
    Sub { v, target }
}
fn unify(ctx: &Context, mut a: Type, mut b: Type) -> Option<Vec<Sub>> {
    Some(match (a, b) {
        (Var(a), Var(b)) if a != b => {
            vec![sub(Var(a), b)]
        }
        (Var(a), t) if !t.contains(ctx, a) => {
            vec![sub(t, a)]
        }
        (t, Var(b)) if !t.contains(ctx, b) => {
            vec![sub(t, b)]
        }
        (a, b) if a == b => {
            vec![]
        }
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
#[derive(Debug)]
pub struct Context {
    pub types: Vec<Option<Type>>,
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
                    Some(sub(Var(self.gen()), *v))
                } else {
                    None
                }
            }
            _ => unreachable!(),
        }
    }
    pub fn dbg(&mut self, exprs: &[Expr], src: &str) {
        for idx in 0..exprs.len() {
            let r = &exprs[idx].range;
            print!("{}: ", src[r.clone()].trim());
            self.types[idx].as_ref().map(|v| v.dbg(self, true));
            println!();
        }
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
            let b = ctx.gen();
            ctx.types[*arg] = Some(Var(b));
            ctx.types[idx] = Some(Base(Base::Fun, vec![Ref(*arg), Ref(*body)]));
            infer(ctx, exprs, *body);
        }
        ExprType::App(e1, e2) => {
            infer(ctx, exprs, *e1);
            infer(ctx, exprs, *e2);
            let b = ctx.gen();
            let alt = Base(Base::Fun, vec![Ref(*e2), Var(b)]);
            ctx.types[idx] = Some(Var(b));
            // Make sure every branch sets ctx.types[idx] to Some when correct or else this fails
            let ty = ctx.types[*e1].clone()?;
            ctx.apply(&unify(ctx, ctx.full_clone(alt)?, ctx.full_clone(ty)?)?);
        }
        ExprType::Let {
            definition,
            var,
            val,
            cont,
        } => {
            ctx.types[*val] = Some(Var(ctx.gen()));
            ctx.types[*var] = if *definition {
                Some(Closed(Box::new(Ref(*val))))
            } else {
                Some(Ref(*val))
            };
            infer(ctx, exprs, *val);
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
