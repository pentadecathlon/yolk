pub type ExprIdx = usize;
#[derive(Debug)]
pub enum ExprType {
    Abs {
        arg: ExprIdx,
        ty: Option<ExprIdx>,
        strict: bool,
        body: ExprIdx,
    },
    App(ExprIdx, ExprIdx),
    Let {
        var: ExprIdx,
        val: ExprIdx,
        cont: ExprIdx,
    },
    Var(Rc<str>),
    Ident(Rc<str>, ExprIdx),
}
#[derive(Debug)]
pub struct Expr {
    pub ty: ExprType,
    pub range: Range,
}
impl Expr {
    fn add(self, v: &mut Vec<Expr>) -> usize {
        v.push(self);
        v.len() - 1
    }
}
fn expr(ty: ExprType, start: usize, end: usize) -> Expr {
    Expr {
        ty,
        range: Range(start, end),
    }
}

use std::rc::Rc;

use ExprType::*;
#[derive(Debug)]
pub struct Range(pub usize, pub usize);
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
    Backslash,
    Assign,
    Ident(Rc<str>),
}
#[derive(Clone, Copy, Debug)]
pub struct ParseState<'a> {
    pub s: &'a str,
    pub start: usize,
}
fn reserved_char(c: char) -> bool {
    c == '\\' || c == '(' || c == ')' || c == '[' || c == ']'
}
impl<'a> ParseState<'a> {
    fn next(self) -> (Option<Token>, Self) {
        let (token, new) = if let Some(s) = self.s.strip_prefix('(') {
            (Token::OpenParen, s)
        } else if let Some(s) = self.s.strip_prefix(')') {
            (Token::CloseParen, s)
        } else if let Some(s) = self.s.strip_prefix('[') {
            (Token::OpenBracket, s)
        } else if let Some(s) = self.s.strip_prefix(']') {
            (Token::CloseBracket, s)
        } else if let Some(s) = self.s.strip_prefix('\\') {
            (Token::Backslash, s)
        } else if let Some(s) = self.s.strip_prefix(',') {
            (Token::Comma, s)
        } else {
            let s = self.s.trim_start_matches(|c: char| {
                !c.is_whitespace() && !c.is_numeric() && !reserved_char(c)
            });
            let dif = self.s.len() - s.len();
            let tok = &self.s[..dif];
            if tok == "=" {
                (Token::Assign, s)
            } else if tok.len() > 0 {
                (Token::Ident(tok.into()), s)
            } else {
                return (None, self);
            }
        };
        let dif = self.s.len() - new.len();
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
            while let Ok((s, e)) = parse(state, scope.clone(), exprs) {
                state = s;
                lead = expr(App(lead, e), 0, 0).add(exprs);
            }
            let (Some(Token::CloseParen), state) = state.next() else {
                return state.err("Expected close paren");
            };
            exprs[lead].range.0 = s.start;
            exprs[lead].range.1 = state.start;
            Ok((state, lead))
        }
        Token::Backslash => {
            let (Some(token), state) = state.next() else {
                return state.err("Lex error");
            };
            let (ident, ty, strict, state) = match token {
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
                    if (strict && t != Token::CloseBracket) || (!strict && t != Token::CloseParen) {
                        return state.err("Expected closing paren or bracket");
                    }
                    let (Some(Token::Ident(ident)), state) = state.next() else {
                        return state.err("Expected identifier");
                    };

                    (ident, Some(lead), strict, state)
                }
                _ => return state.err("Expected argument or type annotation"),
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
            if let (Some(Token::Assign), state) = state.next() {
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
        _ => state.err("Unexpected token"),
    }
}
