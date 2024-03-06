#[derive(Debug)]
pub enum ExprType {
    Abs {
        arg: String,
        ty: Option<Box<Expr>>,
        strict: bool,
        body: Box<Expr>,
    },
    App(Box<Expr>, Box<Expr>),
    Let {
        var: String,
        val: Box<Expr>,
        cont: Box<Expr>,
    },
    Ident(String),
}
#[derive(Debug)]
pub struct Expr {
    pub ty: ExprType,
    pub range: Range,
}
fn expr(ty: ExprType, start: usize, end: usize) -> Expr {
    Expr {
        ty,
        range: Range(start, end),
    }
}

use ExprType::*;
#[derive(Debug)]
pub struct Range(usize, usize);
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
    Ident(String),
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
                (Token::Ident(tok.to_string()), s)
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
pub fn parse(s: ParseState) -> Result<(ParseState, Expr), ParseError> {
    let (Some(token), state) = s.next() else {
        return s.err("Lex error");
    };
    match token {
        Token::OpenParen => {
            let (mut state, mut lead) = parse(state)?;
            while let Ok((s, e)) = parse(state) {
                state = s;
                lead = expr(App(Box::new(lead), Box::new(e)), s.start, state.start);
            }
            let (Some(Token::CloseParen), state) = state.next() else {
                return state.err("Expected close paren");
            };
            Ok((state, lead))
        }
        Token::Backslash => {
            let (Some(token), state) = state.next() else {
                return state.err("Lex error");
            };
            let (id, ty, strict, state) = match token {
                Token::Ident(id) => (id, None, false, state),
                Token::OpenParen | Token::OpenBracket => {
                    let (mut state, mut lead) = parse(state)?;
                    while let Ok((s, e)) = parse(state) {
                        state = s;
                        lead = expr(App(Box::new(lead), Box::new(e)), s.start, state.start);
                    }
                    let (Some(t), state) = state.next() else {
                        return state.err("Lex error");
                    };
                    let strict = token == Token::OpenBracket;
                    if (strict && t != Token::CloseBracket) || (!strict && t != Token::CloseParen) {
                        return state.err("Expected closing paren or bracket");
                    }
                    let (Some(Token::Ident(id)), state) = state.next() else {
                        return state.err("Expected identifier");
                    };

                    (id, Some(Box::new(lead)), strict, state)
                }
                _ => return state.err("Expected argument or type annotation"),
            };
            let (state, body) = parse(state)?;
            Ok((
                state,
                expr(
                    Abs {
                        arg: id,
                        ty,
                        strict,
                        body: Box::new(body),
                    },
                    s.start,
                    state.start,
                ),
            ))
        }
        Token::Ident(id) => {
            if let (Some(Token::Assign), state) = state.next() {
                let (state, body) = parse(state)?;
                let (state, cont) = parse(state)?;
                Ok((
                    state,
                    expr(
                        Let {
                            var: id,
                            val: Box::new(body),
                            cont: Box::new(cont),
                        },
                        s.start,
                        state.start,
                    ),
                ))
            } else {
                Ok((state, expr(Ident(id), s.start, state.start)))
            }
        }
        _ => state.err("Unexpected token"),
    }
}
