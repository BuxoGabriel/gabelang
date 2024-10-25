use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
pub enum TOKENTYPE {
    EQUAL,
    PLUS,
    MINUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LET,
    FN,
    NUMBER,
    IDENTIFIER,
    ILLEGAL
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    token_type: TOKENTYPE,
    literal: String
}

impl Token {
    pub fn new(token_type: TOKENTYPE, literal: String) -> Self {
        Self { token_type, literal }
    }
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
        while let Some(token) = self.get_next_token() {
            tokens.push(token);
        }
        tokens
    }
    
    fn get_next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let token_type: TOKENTYPE;
        let literal: String;
        if let Some(c) = self.get_next_char() {
            // There is a next char
            if is_alpha(c) {
                literal = self.read_identifier(c);
                if let Some(tt) = literal_is_keyword(&literal) {
                    token_type = tt;
                } else {
                    token_type = TOKENTYPE::IDENTIFIER;
                }
            }
            else if c.is_numeric() {
                literal = self.read_number(c);
                token_type = TOKENTYPE::NUMBER;
            } else {
                literal = String::from(c);
                token_type = match c {
                    '=' => TOKENTYPE::EQUAL,
                    '+' => TOKENTYPE::PLUS,
                    '-' => TOKENTYPE::MINUS,
                    ',' => TOKENTYPE::COMMA,
                    ';' => TOKENTYPE::SEMICOLON,
                    '(' => TOKENTYPE::LPAREN,
                    ')' => TOKENTYPE::RPAREN,
                    '{' => TOKENTYPE::LBRACKET,
                    '}' => TOKENTYPE::RBRACKET,
                    _ => TOKENTYPE::ILLEGAL
                };
            }
            Some(Token::new(token_type, literal))
        } else {
            // There are no chars left
            None
        }
    }

    fn get_next_char(&mut self) -> Option<char> {
        self.peekable_iter.next()
    }

    fn peek_next_char(&mut self) -> Option<&char> {
        self.peekable_iter.peek()
    }

    fn read_identifier(&mut self, first_char: char) -> String {
        let mut identifier = String::new();
        identifier.push(first_char);
        while let Some(char) = self.peek_next_char() {
            if is_alpha(*char) {
                identifier.push(self.get_next_char().unwrap());
            } else {
                break;
            }
        }
        identifier
    }

    fn read_number(&mut self, first_num: char) -> String {
        let mut number = String::new();
        number.push(first_num);
        while let Some(c) = self.peek_next_char() {
            if c.is_numeric() {
                number.push(self.get_next_char().unwrap());
            } else {
                break;
            }
        }
        number
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_next_char() {
            if c.is_whitespace() {
                self.get_next_char();
            } else {
                break
            }
        }
    }
}

fn is_alpha(c: char) -> bool {
    ('a' <= c && c <= 'z') ||
    ('A' <= c && c <= 'Z') ||
    c == '_'
}

fn literal_is_keyword(literal: &String) -> Option<TOKENTYPE> {
    match literal.as_str() {
        "let" => Some(TOKENTYPE::LET),
        "fn" => Some(TOKENTYPE::FN),
        _ => None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_number() {
        let input = String::from("1");
        let expected_tokens = vec![Token::new(TOKENTYPE::NUMBER, String::from("1"))];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_multiple_numbers() {
        let input = String::from("123 323 111");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::NUMBER, String::from("123")),
            Token::new(TOKENTYPE::NUMBER, String::from("323")),
            Token::new(TOKENTYPE::NUMBER, String::from("111"))
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_identifiers() {
        let input = String::from("Hello my name is b_ob");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::IDENTIFIER, String::from("Hello")),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("my")),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("name")),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("is")),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("b_ob"))
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_eq() {
        let input = String::from('=');
        let expected_tokens = vec![Token::new(TOKENTYPE::EQUAL, String::from('='))];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_plus() {
        let input = String::from('+');
        let expected_tokens = vec![Token::new(TOKENTYPE::PLUS, String::from('+'))];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_minus() {
        let input = String::from('-');
        let expected_tokens = vec![Token::new(TOKENTYPE::MINUS, String::from('-'))];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_illegal() {
        let input = String::from('~');
        let expected_tokens = vec![Token::new(TOKENTYPE::ILLEGAL, String::from('~'))];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_everything() {
        let input = String::from("fn plus_one(foo) {\n\tlet number = foo + 1; number\n}");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::FN, String::from("fn")),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("plus_one")),
            Token::new(TOKENTYPE::LPAREN, String::from('(')),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("foo")),
            Token::new(TOKENTYPE::RPAREN, String::from(')')),
            Token::new(TOKENTYPE::LBRACKET, String::from('{')),
            Token::new(TOKENTYPE::LET, String::from("let")),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("number")),
            Token::new(TOKENTYPE::EQUAL, String::from('=')),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("foo")),
            Token::new(TOKENTYPE::PLUS, String::from('+')),
            Token::new(TOKENTYPE::NUMBER, String::from("1")),
            Token::new(TOKENTYPE::SEMICOLON, String::from(';')),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("number")),
            Token::new(TOKENTYPE::RBRACKET, String::from('}')),
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }
}
