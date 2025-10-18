use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TokenList {
    tokens: Vec<Token>,
}

impl TokenList {
    pub fn generate(code: String) -> anyhow::Result<Self> {
        let mut tokens: Vec<Token> = Vec::new();
        let words: Vec<&str> = code.split_whitespace().collect();
        for w in words.iter() {
            let chars: Vec<char> = w.chars().collect();
            tokens.append(&mut Self::tokens_in_word(&chars[..]));
        }

        Ok(Self { tokens })
    }

    pub fn tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }

    fn tokens_in_word(word: &[char]) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        match word {
            ['(', ')'] => {
                tokens.push(Token::Literal(Literal::Unit));
            }

            [start @ .., ']'] => {
                tokens.append(&mut Self::tokens_in_word(start));
                tokens.push(Token::RBracket);
            }
            ['[', rest @ ..] => {
                tokens.push(Token::LBracket);
                tokens.append(&mut Self::tokens_in_word(rest));
            }

            [start @ .., ')'] => {
                tokens.append(&mut Self::tokens_in_word(start));
                tokens.push(Token::RParen);
            }
            ['(', rest @ ..] => {
                tokens.push(Token::LParen);
                tokens.append(&mut Self::tokens_in_word(rest));
            }

            ['"', middle @ .., '"'] => {
                tokens.push(Token::Literal(Literal::String(middle.iter().collect())));
            }
            ['-', '>'] => {
                tokens.push(Token::Arrow);
            }
            [':'] => {
                tokens.push(Token::Colon);
            }
            w => {
                if let Some(keyword) = Keyword::from_word(w) {
                    tokens.push(Token::Keyword(keyword));
                } else if let Some(literal) = Literal::from_word(w) {
                    tokens.push(Token::Literal(literal));
                } else if let Some(operator) = Operator::from_word(w) {
                    tokens.push(Token::Operator(operator));
                } else if w.len() != 0 {
                    tokens.push(Token::Identifier(w.iter().collect()));
                }
            }
        }

        tokens
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Operator(Operator),
    Identifier(String),
    Literal(Literal),
    Arrow, // ->
    Colon, // :
    LParen,
    RParen,
    LBracket,
    RBracket,
    EOF,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq)]
pub enum Keyword {
    Let,
    In,
    Fn,
    If,
    Then,
    Else,
}

impl Keyword {
    fn from_word(word: &[char]) -> Option<Self> {
        let s: String = word.iter().collect();

        Some(match s.as_str() {
            "let" => Self::Let,
            "in" => Self::In,
            "fn" => Self::Fn,
            "if" => Self::If,
            "then" => Self::Then,
            "else" => Self::Else,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq)]
pub enum Operator {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Eq,  // =
}

impl Operator {
    fn from_word(word: &[char]) -> Option<Self> {
        let s: String = word.iter().collect();

        Some(match s.as_str() {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "=" => Self::Eq,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String), // a string enclosed by "quotes"
    Bool(bool),
    Unit, // ()
}

impl Literal {
    fn from_word(word: &[char]) -> Option<Self> {
        let s: String = word.iter().collect();
        Some(if let Ok(int) = s.parse::<i64>() {
            Self::Int(int)
        } else if let Ok(float) = s.parse::<f64>() {
            Self::Float(float)
        } else if let Ok(b) = s.parse::<bool>() {
            Self::Bool(b)
        } else if s.as_str() == "()" {
            Self::Unit
        } else {
            return None;
        })
    }
}
