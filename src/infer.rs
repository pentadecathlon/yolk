use crate::parse::*;
type TypeVar = u64;
type Var = u64;

#[derive(PartialEq, Debug, Clone)]
pub enum Base {
    Fun,
    Sum,
    Float,
    Int,
}
#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Gen(TypeVar, Box<Type>),
    Var(TypeVar),
    Base(Base, Vec<Type>),
}
use Type::*;
impl Type {
    fn contains(&self, var: u64) -> bool {
        match self {
            Var(v) => *v == var,
            Base(_, args) => args.iter().any(|t| t.contains(var)),
            Gen(_, t) => t.contains(var),
        }
    }
    fn apply(&mut self, sub: &Sub) {
        match self {
            Var(v) => {
                if *v == sub.target {
                    *self = sub.v.clone()
                }
            }
            Base(_, args) => {
                for t in args.iter_mut() {
                    t.apply(sub)
                }
            }
            Gen(_, t) => t.apply(sub),
        }
    }
    fn instance(mut self, id: u64) -> (u64, Self) {
        match self {
            Gen(v, t) => {
                self.apply(&sub(Var(id), v));
                t.instance(id + 1)
            }
            _ => (id, self),
        }
    }
    fn apply_all(mut self, subs: &Vec<Sub>) -> Self {
        for sub in subs.iter().rev() {
            self.apply(sub)
        }
        self
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
fn unify(a: Type, b: Type) -> Option<Vec<Sub>> {
    Some(match (a, b) {
        (Var(a), Var(b)) if a != b => {
            vec![sub(Var(a), b)]
        }
        (Var(a), t) if !t.contains(a) => {
            vec![sub(t, a)]
        }
        (t, Var(b)) if !t.contains(b) => {
            vec![sub(t, b)]
        }
        (a, b) if a == b => {
            vec![]
        }
        (Base(a, mut l1), Base(b, mut l2)) if l1.len() == l2.len() => {
            let u_1 = unify(l1.pop()?, l2.pop()?)?;
            let mut u = unify(Base(a, l1).apply_all(&u_1), Base(b, l2).apply_all(&u_1))?;
            u.extend_from_slice(&u_1);
            u
        }
        _ => return None,
    })
}
