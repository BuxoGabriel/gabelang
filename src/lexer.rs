use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TOKENTYPE {
    EQUAL,
    BANG,
    EQ,
    NOTEQ,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    LT,
    GT,
    COMMA,
    COLON,
    DOT,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LSQUIG,
    RSQUIG,
    LSQR,
    RSQR,
    LET,
    FN,
    WHILE,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
    NUMBER,
    IDENTIFIER,
    ILLEGAL
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    line: usize,
    position: usize
}

impl Default for Location {
    fn default() -> Self {
        Self {
            line: 1,
            position: 1
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TOKENTYPE,
    pub literal: String,
    pub location: Location
}

impl Token {
    pub fn new(token_type: TOKENTYPE, literal: String, location: Location) -> Self {
        Self { token_type, literal, location}
    }
}

pub struct Lexer<'a> {
    peekable_iter: Peekable<Chars<'a>>,
    location: Location
}

impl<'a> Lexer<'a> {
    pub fn new(contents: &'a str) -> Lexer<'a> {
        Self {
            peekable_iter: contents.chars().peekable(),
            location: Location::default()
        }
    }

    pub fn parse(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.get_next_token() {
            tokens.push(token);
        }
        tokens
    }
    
    pub fn get_next_token(&mut self) -> Option<Token> {
        // Whitespace is not part of the language so skip it but it can delimit tokens
        self.skip_whitespace();
        let token_type: TOKENTYPE;
        let mut literal: String;
        let location = self.location.clone();
        let current_char = self.get_next_char();
        // If no chars are left return None for EOF
        if current_char.is_none() {
            return None
        }
        let current_char = current_char.unwrap();
        // There is a next char
        if Lexer::is_alpha(current_char) {
            literal = self.read_identifier(current_char);
            token_type = Lexer::literal_keyword(&literal);
        }
        else if current_char.is_numeric() {
            literal = self.read_number(current_char);
            token_type = TOKENTYPE::NUMBER;
        } else {
            literal = String::from(current_char);
            token_type = match current_char {
                '=' => {
                    if let Some(next_char) = self.peek_next_char() {
                        if *next_char == '=' {
                            literal = String::from("==");
                            self.get_next_char();
                            TOKENTYPE::EQ
                        } else {
                            TOKENTYPE::EQUAL
                        }
                    } else {
                        TOKENTYPE::EQUAL
                    }
                }
                '!' => {
                    if let Some(next_char) = self.peek_next_char() {
                        if *next_char == '=' {
                            literal = String::from("!=");
                            self.get_next_char();
                            TOKENTYPE::NOTEQ
                        } else {
                            TOKENTYPE::BANG
                        }
                    } else {
                        TOKENTYPE::BANG
                    }
                }
                '+' => TOKENTYPE::PLUS,
                '-' => TOKENTYPE::MINUS,
                '*' => TOKENTYPE::ASTERISK,
                '/' => TOKENTYPE::SLASH,
                '<' => TOKENTYPE::LT,
                '>' => TOKENTYPE::GT,
                ',' => TOKENTYPE::COMMA,
                '.' => TOKENTYPE::DOT,
                ':' => TOKENTYPE::COLON,
                ';' => TOKENTYPE::SEMICOLON,
                '(' => TOKENTYPE::LPAREN,
                ')' => TOKENTYPE::RPAREN,
                '{' => TOKENTYPE::LSQUIG,
                '}' => TOKENTYPE::RSQUIG,
                '[' => TOKENTYPE::LSQR,
                ']' => TOKENTYPE::RSQR,
                _ => TOKENTYPE::ILLEGAL
            };
        }
        Some(Token::new(token_type, literal, location))
    }

    fn get_next_char(&mut self) -> Option<char> {
        self.location.position += 1;
        self.peekable_iter.next()
    }

    fn peek_next_char(&mut self) -> Option<&char> {
        self.peekable_iter.peek()
    }

    fn read_identifier(&mut self, first_char: char) -> String {
        let mut identifier = String::new();
        identifier.push(first_char);
        while let Some(char) = self.peek_next_char() {
            if Lexer::is_alpha(*char) {
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
                let is_new_line = *c == '\n';
                self.get_next_char();
                if is_new_line {
                    self.location.line += 1;
                    self.location.position = 1;
                }
            } else {
                break
            }
        }
    }

    fn is_alpha(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn literal_keyword(literal: &String) -> TOKENTYPE {
        match literal.as_str() {
            "let" => TOKENTYPE::LET,
            "return" =>TOKENTYPE::RETURN,
            "fn" => TOKENTYPE::FN,
            "while" => TOKENTYPE::WHILE,
            "if" => TOKENTYPE::IF,
            "else" => TOKENTYPE::ELSE,
            "True" => TOKENTYPE::TRUE,
            "False" => TOKENTYPE::FALSE,
        _ => TOKENTYPE::IDENTIFIER
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_numbers() {
        let input = String::from("123 323 111");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::NUMBER, String::from("123"), Location::default()),
            Token::new(TOKENTYPE::NUMBER, String::from("323"), Location { line: 1, position: 5 }),
            Token::new(TOKENTYPE::NUMBER, String::from("111"), Location { line: 1, position: 9 }),
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_identifiers() {
        let input = String::from("Hello wo_rld");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::IDENTIFIER, String::from("Hello"), Location::default()),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("wo_rld"), Location { line: 1, position: 7 }),
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_symbols() {
        let input = String::from("=+-*/,:;()[]{}<>!");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::EQUAL, String::from('='), Location::default()),
            Token::new(TOKENTYPE::PLUS, String::from('+'), Location { line: 1, position: 2 }),
            Token::new(TOKENTYPE::MINUS, String::from('-'), Location { line: 1, position: 3 }),
            Token::new(TOKENTYPE::ASTERISK, String::from('*'), Location { line: 1, position: 4 }),
            Token::new(TOKENTYPE::SLASH, String::from('/'), Location { line: 1, position: 5 }),
            Token::new(TOKENTYPE::COMMA, String::from(','), Location { line: 1, position: 6 }),
            Token::new(TOKENTYPE::COLON, String::from(':'), Location { line: 1, position: 7 }),
            Token::new(TOKENTYPE::SEMICOLON, String::from(';'), Location { line: 1, position: 8 }),
            Token::new(TOKENTYPE::LPAREN, String::from('('), Location { line: 1, position: 9 }),
            Token::new(TOKENTYPE::RPAREN, String::from(')'), Location { line: 1, position: 10 }),
            Token::new(TOKENTYPE::LSQR, String::from('['), Location { line: 1, position: 11 }),
            Token::new(TOKENTYPE::RSQR, String::from(']'), Location { line: 1, position: 12 }),
            Token::new(TOKENTYPE::LSQUIG, String::from('{'), Location { line: 1, position: 13 }),
            Token::new(TOKENTYPE::RSQUIG, String::from('}'), Location { line: 1, position: 14 }),
            Token::new(TOKENTYPE::LT, String::from('<'), Location { line: 1, position: 15 }),
            Token::new(TOKENTYPE::GT, String::from('>'), Location { line: 1, position: 16 }),
            Token::new(TOKENTYPE::BANG, String::from('!'), Location { line: 1, position: 17 }),
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_illegal() {
        let input = String::from('~');
        let expected_tokens = vec![Token::new(TOKENTYPE::ILLEGAL, String::from('~'), Location::default())];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_keywords() {
        let input = String::from("let fn while if else return True False");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::LET, String::from("let"), Location::default()),
            Token::new(TOKENTYPE::FN, String::from("fn"), Location { line: 1, position: 5 }),
            Token::new(TOKENTYPE::WHILE, String::from("while"), Location{ line: 1, position: 8 }),
            Token::new(TOKENTYPE::IF, String::from("if"), Location { line: 1, position: 14 }),
            Token::new(TOKENTYPE::ELSE, String::from("else"), Location{ line: 1, position: 17 }),
            Token::new(TOKENTYPE::RETURN, String::from("return"), Location{ line: 1, position: 22 }),
            Token::new(TOKENTYPE::TRUE, String::from("True"), Location{ line: 1, position: 29 }),
            Token::new(TOKENTYPE::FALSE, String::from("False"), Location{ line: 1, position: 34 }),
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(tokens, expected_tokens)
    }

    #[test]
    fn parse_everything() {
        let input = String::from("fn plus_one(foo) {\n\tlet number = foo + 1; number\n}");
        let expected_tokens = vec![
            Token::new(TOKENTYPE::FN, String::from("fn"), Location::default()),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("plus_one"), Location { line: 1, position: 4 }),
            Token::new(TOKENTYPE::LPAREN, String::from('('), Location { line: 1, position: 12 }),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("foo"), Location { line: 1, position: 13 }),
            Token::new(TOKENTYPE::RPAREN, String::from(')'), Location { line: 1, position: 16 }),
            Token::new(TOKENTYPE::LSQUIG, String::from('{'), Location { line: 1, position: 18 }),
            Token::new(TOKENTYPE::LET, String::from("let"), Location { line: 2, position: 2 }),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("number"), Location { line: 2, position: 6 }),
            Token::new(TOKENTYPE::EQUAL, String::from('='), Location { line: 2, position: 13 }),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("foo"), Location { line: 2, position: 15 }),
            Token::new(TOKENTYPE::PLUS, String::from('+'), Location { line: 2, position: 19 }),
            Token::new(TOKENTYPE::NUMBER, String::from("1"), Location { line: 2, position: 21 }),
            Token::new(TOKENTYPE::SEMICOLON, String::from(';'), Location { line: 2, position: 22 }),
            Token::new(TOKENTYPE::IDENTIFIER, String::from("number"), Location { line: 2, position: 24 }),
            Token::new(TOKENTYPE::RSQUIG, String::from('}'), Location { line: 3, position: 1 }),
        ];
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }
}
