use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
pub enum TOKENTYPE {
    INT,
    EQUAL,
    NUMBER,
    IDENTIFIER,
    EOF,
    ILLEGAL
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    token_type: TOKENTYPE,
    literal: String
}

pub struct Lexer<'a> {
    input: &'a str,
    peekable_iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(contents: &'a str) -> Lexer<'a> {
        Self {
            input: contents,
            peekable_iter: contents.chars().peekable()
        }
    }
    pub fn parse(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        tokens.push(self.get_next_token());
        tokens
    }
    fn get_next_token(&mut self) -> Token {
        if let Some(char) = self.get_next_char() {
            let token_type = match char {
                '=' => TOKENTYPE::EQUAL,
                '\0' => TOKENTYPE::EOF,
                _ => TOKENTYPE::ILLEGAL
            };
            return Token {
                token_type,
                literal: String::from(char)
            }
        } else {
            return Token {
                token_type: TOKENTYPE::EOF,
                literal: String::new()
            }
        }
    }

    fn get_next_char(&mut self) -> Option<char> {
        self.peekable_iter.next()
    }

    fn peek_next_char(&mut self) -> Option<&char> {
        self.peekable_iter.peek()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parseOne() {
        let input = String::from("1");
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        let expected_token = Token {
            token_type: TOKENTYPE::NUMBER,
            literal: String::from("1")
        };
        assert_eq!(vec![expected_token], tokens)
    }
}
