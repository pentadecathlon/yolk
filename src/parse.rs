pub type ExprIdx = usize;
#[derive(Debug)]
pub enum Literal {
    Float(f64),
    Int(i64),
}
#[derive(Debug)]
pub enum ExprType {
    // lambda (abstraction)
    Abs {
        arg: ExprIdx,
        // Not used yet, for annotations, later
        ty: Option<ExprIdx>,
        // uhhh thing (? also not used yet)
        strict: bool,
        body: ExprIdx,
    },
    // Application
    App(ExprIdx, ExprIdx),
    Let {
        definition: bool,
        var: ExprIdx,
        val: ExprIdx,
        cont: ExprIdx,
    },
    // Not really an expression but we store variables as expressions for typing reasons
    Var(Rc<str>),
    Ident(Rc<str>, ExprIdx),
    Lit(Literal),
}
#[derive(Debug)]
pub struct Expr {
    pub ty: ExprType,
    pub range: Range<usize>,
}
impl Expr {
    fn add(self, v: &mut Vec<Expr>) -> usize {
        v.push(self);
        v.len() - 1
    }
}
pub fn expr(ty: ExprType, start: usize, end: usize) -> Expr {
    Expr {
        ty,
        range: Range { start, end },
    }
}

use std::{ops::Range, rc::Rc};

use ExprType::*;
#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    pub pos: usize,
}
#[derive(Clone, Debug, PartialEq)]
enum Token {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Comma,
    // bool for whether it has a named arg or not
    Abs(bool),
    // bool for whether its polymorphic
    Assign(bool),
    Ident(Rc<str>),
    Int(i64),
    Float(f64),
}
#[derive(Clone, Copy, Debug)]
pub struct ParseState<'a> {
    pub s: &'a str,
    pub start: usize,
}
fn reserved_char(c: char) -> bool {
    c == '\\' || c == '(' || c == ')' || c == '[' || c == ']' || c == '~'
}
fn read_char(c: char, s: &str) -> Option<&str> {
    s.strip_prefix(c)
}
fn read_ident(s: &str) -> Option<(Rc<str>, &str)> {
    if !s.starts_with(|c: char| !c.is_numeric()) {
        return None;
    }
    let cut = s.trim_start_matches(|c| !reserved_char(c) && !c.is_whitespace());
    if cut.is_empty() {
        return None;
    }
    let dif = s.len() - cut.len();
    Some((s[0..dif].into(), cut))
}
fn read_float(s: &str) -> Option<(f64, &str)> {
    let cut = read_char('-', s);
    let cut = cut.unwrap_or(s);
    let cut = cut.trim_start_matches(|c: char| c.is_numeric() || c == '.');
    let dif = s.len() - cut.len();
    if !s[0..dif].contains('.') {
        return None;
    }
    Some((s[0..dif].parse::<f64>().ok()?, cut))
}
fn read_int(s: &str) -> Option<(i64, &str)> {
    let cut = read_char('-', s);
    let cut = cut.unwrap_or(s);
    let cut = cut.trim_start_matches(|c: char| c.is_numeric());
    let dif = s.len() - cut.len();
    Some((s[0..dif].parse::<i64>().ok()?, cut))
}
impl<'a> ParseState<'a> {
    fn next(self) -> (Option<Token>, Self) {
        let (token, new) = if let Some(s) = read_char('(', self.s) {
            (Token::OpenParen, s)
        } else if let Some(s) = read_char(')', self.s) {
            (Token::CloseParen, s)
        } else if let Some(s) = read_char('[', self.s) {
            (Token::OpenBracket, s)
        } else if let Some(s) = read_char(']', self.s) {
            (Token::CloseBracket, s)
        } else if let Some(s) = read_char('\\', self.s) {
            (Token::Abs(true), s)
        } else if let Some(s) = read_char('~', self.s) {
            (Token::Abs(false), s)
        } else if let Some(s) = read_char(',', self.s) {
            (Token::Comma, s)
        } else if let Some((v, s)) = read_float(self.s) {
            (Token::Float(v), s)
        } else if let Some((v, s)) = read_int(self.s) {
            (Token::Int(v), s)
        } else if let Some((ident, s)) = read_ident(self.s) {
            (
                if &*ident == "=" {
                    Token::Assign(false)
                } else if &*ident == ":" {
                    Token::Assign(true)
                } else {
                    Token::Ident(ident)
                },
                s,
            )
        } else {
            return (None, self);
        };
        let dif = self.s.len() - new.trim_start().len();
        (
            Some(dbg!(token)),
            Self {
                s: new.trim_start(),
                start: self.start + dif,
            },
        )
    }
    fn err<T>(&self, s: &str) -> Result<T, ParseError> {
        Err(ParseError {
            msg: s.to_string(),
            pos: self.start,
        })
    }
}
#[derive(Clone)]
pub struct Scope<'a> {
    pub prev: Option<&'a Scope<'a>>,
    pub var: Rc<str>,
    pub id: usize,
}
impl<'a> Scope<'a> {
    pub fn keyword(&'a self, word: &str, exprs: &mut Vec<Expr>) -> Self {
        self.extend(expr(Var(word.into()), 0, 0), exprs)
    }
    fn extend(&'a self, var: Expr, v: &mut Vec<Expr>) -> Self {
        let Var(name) = &var.ty else {
            panic!("Not a var")
        };
        let name = name.clone();
        var.add(v);
        Scope {
            prev: Some(self),
            var: name,
            id: v.len() - 1,
        }
    }
    fn get(&self, var: &str) -> Option<usize> {
        if &*self.var == var {
            Some(self.id)
        } else {
            self.prev?.get(var)
        }
    }
}
pub fn parse<'a>(
    s: ParseState<'a>,
    scope: Scope,
    exprs: &mut Vec<Expr>,
) -> Result<(ParseState<'a>, ExprIdx), ParseError> {
    let (Some(token), state) = s.next() else {
        return s.err("Lex error");
    };
    match token {
        Token::OpenParen => {
            let (mut state, mut lead) = parse(state, scope.clone(), exprs)?;
            while let Ok((next_state, e)) = parse(state, scope.clone(), exprs) {
                state = next_state;
                lead = expr(App(lead, e), s.start + 1, state.start).add(exprs);
            }
            let (Some(Token::CloseParen), state) = state.next() else {
                return state.err("Expected close paren");
            };
            exprs[lead].range.start = s.start;
            exprs[lead].range.end = state.start;
            Ok((state, lead))
        }
        Token::Abs(has_arg) => {
            let (ident, ty, strict, state) = if has_arg {
                let (Some(token), state) = state.next() else {
                    return state.err("Lex error");
                };
                match token {
                    Token::Ident(id) => (id, None, false, state),
                    Token::OpenParen | Token::OpenBracket => {
                        let (mut state, mut lead) = parse(state, scope.clone(), exprs)?;
                        while let Ok((s, e)) = parse(state, scope.clone(), exprs) {
                            state = s;
                            lead = expr(App(lead, e), s.start, state.start).add(exprs);
                        }
                        let (Some(t), state) = state.next() else {
                            return state.err("Lex error");
                        };
                        let strict = token == Token::OpenBracket;
                        if (strict && t != Token::CloseBracket)
                            || (!strict && t != Token::CloseParen)
                        {
                            return state.err("Expected closing paren or bracket");
                        }
                        let (Some(Token::Ident(ident)), state) = state.next() else {
                            return state.err("Expected identifier");
                        };

                        (ident, Some(lead), strict, state)
                    }
                    _ => return state.err("Expected argument or type annotation"),
                }
            } else {
                ("".into(), None, false, state)
            };
            let scope = scope.extend(
                expr(Var(ident.clone()), s.start + 1, s.start + ident.len() + 1),
                exprs,
            );
            let idx = scope.id;
            let (state, body) = parse(state, scope, exprs)?;
            Ok((
                state,
                expr(
                    Abs {
                        arg: idx,
                        ty,
                        strict,
                        body,
                    },
                    s.start,
                    state.start,
                )
                .add(exprs),
            ))
        }
        Token::Ident(ident) => {
            if let (Some(Token::Assign(definition)), state) = state.next() {
                let scope = scope.extend(
                    expr(Var(ident.clone()), s.start, s.start + ident.len()),
                    exprs,
                );
                let idx = scope.id;
                let (state, body) = parse(state, scope.clone(), exprs)?;
                let (state, cont) = parse(state, scope, exprs)?;
                Ok((
                    state,
                    expr(
                        Let {
                            definition,
                            var: idx,
                            val: body,
                            cont,
                        },
                        s.start,
                        state.start,
                    )
                    .add(exprs),
                ))
            } else {
                let Some(idx) = scope.get(&*ident) else {
                    return state.err("Undeclared");
                };
                Ok((
                    state,
                    expr(Ident(ident, idx), s.start, state.start).add(exprs),
                ))
            }
        }
        Token::Int(v) => Ok((
            state,
            expr(Lit(Literal::Int(v)), s.start, state.start).add(exprs),
        )),
        Token::Float(v) => Ok((
            state,
            expr(Lit(Literal::Float(v)), s.start, state.start).add(exprs),
        )),
        _ => state.err("Unexpected token"),
    }
}
