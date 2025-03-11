use std::fmt::Display;
use std::iter::Peekable;
use std::num::ParseIntError;
use std::str::Chars;

/// An enum over all valid token types in the language
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    /// Maps to =
    EQUAL,
    /// Maps to !
    BANG,
    /// Maps to ==
    EQ,
    /// Maps to !=
    NOTEQ,
    /// Maps to +
    PLUS,
    /// Maps to -
    MINUS,
    /// Maps to *
    ASTERISK,
    /// Maps to /
    SLASH,
    /// Maps to <
    LT,
    /// Maps to >
    GT,
    /// Maps to ,
    COMMA,
    /// Maps to :
    COLON,
    /// Maps to .
    DOT,
    /// Maps to ;
    SEMICOLON,
    /// Maps to all characters inside of two double quotes
    STRING(String),
    /// Maps to (
    LPAREN,
    /// Maps to )
    RPAREN,
    /// Maps to {
    LSQUIG,
    /// Maps to }
    RSQUIG,
    /// Maps to [
    LSQR,
    /// Maps to ]
    RSQR,
    /// Maps to let
    LET,
    /// Maps to fn
    FN,
    /// Maps to while
    WHILE,
    /// Maps to if
    IF,
    /// Maps to else
    ELSE,
    /// Maps to return
    RETURN,
    /// Maps to true
    TRUE,
    /// Maps to false
    FALSE,
    /// Maps to any positive integer
    INT(isize),
    /// Maps to any label that starts with an alphabetical character and only contains alphabetical
    /// characters and underscores
    ///
    /// # Examples
    ///
    /// - ident
    /// - array_list
    /// - my_obj
    ///
    /// # Invalid examples
    ///
    /// - _my_var
    /// - var1
    IDENTIFIER(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EQUAL => f.write_str("="),
            Self::BANG => f.write_str("!"),
            Self::EQ => f.write_str("=="),
            Self::NOTEQ => f.write_str("!="),
            Self::PLUS => f.write_str("+"),
            Self::MINUS => f.write_str("-"),
            Self::ASTERISK => f.write_str("*"),
            Self::SLASH => f.write_str("/"),
            Self::LT => f.write_str("<"),
            Self::GT => f.write_str(">"),
            Self::COMMA => f.write_str(","),
            Self::COLON => f.write_str(":"),
            Self::DOT => f.write_str("."),
            Self::SEMICOLON => f.write_str(";"),
            Self::STRING(string) => write!(f, "\"{}\"", string),
            Self::LPAREN => f.write_str("("),
            Self::RPAREN => f.write_str(")"),
            Self::LSQUIG => f.write_str("{"),
            Self::RSQUIG => f.write_str("}"),
            Self::LSQR => f.write_str("["),
            Self::RSQR => f.write_str("]"),
            Self::LET => f.write_str("let"),
            Self::FN => f.write_str("fn"),
            Self::WHILE => f.write_str("while"),
            Self::IF => f.write_str("if"),
            Self::ELSE => f.write_str("else"),
            Self::RETURN => f.write_str("return"),
            Self::TRUE => f.write_str("true"),
            Self::FALSE => f.write_str("false"),
            Self::INT(val) => write!(f, "Int({})", val),
            Self::IDENTIFIER(string) => write!(f, "IDENTIFIER({})", string),
        }
    }
}

/// A location in a file or string input 
#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    /// The line of the string as delimited by newline characters
    pub line: usize,
    /// The position in the string, starting at one on the first character of a line and incremented by 1 for every
    /// character, including whitespace
    pub position: usize
}

impl Default for Location {
    fn default() -> Self {
        Self {
            line: 1,
            position: 1
        }
    }
}

/// A combination of a Token and its location in the input string
#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithLocation(Token, Location);

impl TokenWithLocation {
    /// Creates a new TokenWithLocation from its token, and location
    pub fn new(token: Token, location: Location) -> Self {
        Self(token, location)
    }

    /// Converts a TokenWithLocation into a Token for ease of use
    pub fn to_token(self) -> Token {
        self.0
    }
    /// Returns a reference to Token component
    pub fn ref_token(&self) -> &Token {
        &self.0
    }
    /// Returns a reference to Location component
    pub fn ref_location(&self) -> &Location {
        &self.1
    }
}

/// Error type when Lexer fails parse input text
#[derive(Debug, PartialEq, Clone)]
pub enum LexerErrorType {
    /// Error indicates failed to parse number in input into an integer
    ParseIntErr(ParseIntError),
    /// Error indicates input has a string that was never closed
    UnclosedString,
    /// Error indicates Lexer encountered unexpected token while processing input
    IllegalToken
}

impl From<ParseIntError> for LexerErrorType {
    fn from(value: ParseIntError) -> Self {
        Self::ParseIntErr(value)
    }
}

impl Display for LexerErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseIntErr(err) => write!(f, "Error parsing number: {err}"),
            Self::UnclosedString => write!(f, "Unclosed string could not be parsed."),
            Self::IllegalToken => write!(f, "Illegal character could not make valid token")
        }
    }
}

/// Error type generated when lexing fails
#[derive(Debug, PartialEq, Clone)]
pub struct LexerError {
    /// The type of error
    pub error_type: LexerErrorType,
    /// The location in the code where the error was discovered
    pub location: Location
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lexer Error at line {} column {}: {}", self.location.line, self.location.position, self.error_type)
    }
}

type LexerResult<T> = Result<T, LexerError>;

/// The lexer creates a token iterator from a string that it is processing
pub struct Lexer<'a> {
    peekable_iter: Peekable<Chars<'a>>,
    location: Location
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer from a string input
    pub fn new(contents: &'a str) -> Lexer<'a> {
        Self {
            peekable_iter: contents.chars().peekable(),
            location: Location::default()
        }
    }

    /// Generates a vec of all tokens parsed from the lexer's string
    pub fn parse(&mut self) -> LexerResult<Vec<TokenWithLocation>> {
        let mut tokens = Vec::new();
        while let Some(token) = self.get_next_token()? {
            tokens.push(token);
        }
        Ok(tokens)
    }

    /// Gets the next token from the input string in a similar way to an iterator
    pub fn get_next_token(&mut self) -> LexerResult<Option<TokenWithLocation>> {
        // Whitespace is not part of the language so skip it but it can delimit tokens
        self.skip_whitespace();
        let token: Token;
        let location = self.location.clone();
        let current_char = self.get_next_char();
        // If no chars are left return None for EOF
        if current_char.is_none() {
            return Ok(None)
        }
        let current_char = current_char.unwrap();
        // There is a next char
        if current_char.is_alphabetic() {
            let literal = self.read_identifier(current_char);
            token = Lexer::literal_keyword(&literal);
        }
        else if current_char.is_numeric() {
            let value = match self.read_number(current_char).parse::<isize>() {
                Ok(num) => num,
                Err(err) => {
                    return Err(self.error(err.into()));
                }
            };
            token = Token::INT(value);
        } else if current_char == '"' {
            let mut string = String::new();
            loop {
                match self.get_next_char() {
                    Some(current_char) => {
                        if current_char == '"' {
                            break;
                        }
                        string.push(current_char);
                    },
                    None => return Err(self.error(LexerErrorType::UnclosedString)),
                };
            }
            token = Token::STRING(string)

        } else {
            token = match current_char {
                '=' => {
                    if let Some(next_char) = self.peek_next_char() {
                        if *next_char == '=' {
                            self.get_next_char();
                            Token::EQ
                        } else {
                            Token::EQUAL
                        }
                    } else {
                        Token::EQUAL
                    }
                }
                '!' => {
                    if let Some(next_char) = self.peek_next_char() {
                        if *next_char == '=' {
                            self.get_next_char();
                            Token::NOTEQ
                        } else {
                            Token::BANG
                        }
                    } else {
                        Token::BANG
                    }
                }
                '+' => Token::PLUS,
                '-' => Token::MINUS,
                '*' => Token::ASTERISK,
                '/' => {
                    if let Some(next_char) = self.peek_next_char() {
                        if *next_char == '/' {
                            self.get_next_char();
                            while let Some(char) = self.peek_next_char() {
                                if *char == '\n' {
                                    break;
                                }
                                self.get_next_char();
                            }
                            return self.get_next_token()
                        } else {
                            Token::SLASH
                        }
                    } else {
                        Token::SLASH
                    }
                },
                '<' => Token::LT,
                '>' => Token::GT,
                ',' => Token::COMMA,
                '.' => Token::DOT,
                ':' => Token::COLON,
                ';' => Token::SEMICOLON,
                '(' => Token::LPAREN,
                ')' => Token::RPAREN,
                '{' => Token::LSQUIG,
                '}' => Token::RSQUIG,
                '[' => Token::LSQR,
                ']' => Token::RSQR,
                _ => return Err(LexerError {
                    error_type: LexerErrorType::IllegalToken,
                    location
                })
            };
        }
        Ok(Some(TokenWithLocation::new(token, location)))
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
            if char.is_alphabetic() || char.is_numeric() || *char == '_'{
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

    fn error(&mut self, err: LexerErrorType) -> LexerError {
        LexerError {
            error_type: err,
            location: self.location.clone()
        }
    }

    fn literal_keyword(literal: &str) -> Token {
        match literal {
            "let" => Token::LET,
            "return" =>Token::RETURN,
            "fn" => Token::FN,
            "while" => Token::WHILE,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "True" => Token::TRUE,
            "False" => Token::FALSE,
        _ => Token::IDENTIFIER(String::from(literal))
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexerResult<TokenWithLocation>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.get_next_token() {
            Ok(val) => match val {
                Some(val) => Some(Ok(val)),
                None => None
            },
            Err(err) => Some(Err(err))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_numbers() {
        let input = String::from("123 323 111");
        let expected_tokens = Ok(vec![
            TokenWithLocation::new(Token::INT(123), Location::default()),
            TokenWithLocation(Token::INT(323), Location { line: 1, position: 5 }),
            TokenWithLocation::new(Token::INT(111), Location { line: 1, position: 9 }),
        ]);
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_identifiers() {
        let input = String::from("Hello w0_rld");
        let expected_tokens = Ok(vec![
            TokenWithLocation::new(Token::IDENTIFIER(String::from("Hello")), Location::default()),
            TokenWithLocation::new(Token::IDENTIFIER(String::from("w0_rld")), Location { line: 1, position: 7 }),
        ]);
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_symbols() {
        let input = String::from("=+-*/,:;()[]{}<>!");
        let expected_tokens = Ok(vec![
            TokenWithLocation::new(Token::EQUAL, Location::default()),
            TokenWithLocation::new(Token::PLUS, Location { line: 1, position: 2 }),
            TokenWithLocation::new(Token::MINUS, Location { line: 1, position: 3 }),
            TokenWithLocation::new(Token::ASTERISK, Location { line: 1, position: 4 }),
            TokenWithLocation::new(Token::SLASH, Location { line: 1, position: 5 }),
            TokenWithLocation::new(Token::COMMA, Location { line: 1, position: 6 }),
            TokenWithLocation::new(Token::COLON, Location { line: 1, position: 7 }),
            TokenWithLocation::new(Token::SEMICOLON, Location { line: 1, position: 8 }),
            TokenWithLocation::new(Token::LPAREN, Location { line: 1, position: 9 }),
            TokenWithLocation::new(Token::RPAREN, Location { line: 1, position: 10 }),
            TokenWithLocation::new(Token::LSQR, Location { line: 1, position: 11 }),
            TokenWithLocation::new(Token::RSQR, Location { line: 1, position: 12 }),
            TokenWithLocation::new(Token::LSQUIG, Location { line: 1, position: 13 }),
            TokenWithLocation::new(Token::RSQUIG, Location { line: 1, position: 14 }),
            TokenWithLocation::new(Token::LT, Location { line: 1, position: 15 }),
            TokenWithLocation::new(Token::GT, Location { line: 1, position: 16 }),
            TokenWithLocation::new(Token::BANG, Location { line: 1, position: 17 }),
        ]);
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn parse_illegal() {
        let input = String::from('~');
        let expected_result = Err(LexerError{
            error_type: LexerErrorType::IllegalToken,
            location: Location { line: 1, position: 1 }
        });
        let mut lexer = Lexer::new(&input);
        assert_eq!(expected_result, lexer.parse());
    }

    #[test]
    fn parse_keywords() {
        let input = String::from("let fn while if else return True False");
        let expected_tokens = Ok(vec![
            TokenWithLocation::new(Token::LET, Location::default()),
            TokenWithLocation::new(Token::FN, Location { line: 1, position: 5 }),
            TokenWithLocation::new(Token::WHILE, Location{ line: 1, position: 8 }),
            TokenWithLocation::new(Token::IF, Location { line: 1, position: 14 }),
            TokenWithLocation::new(Token::ELSE, Location{ line: 1, position: 17 }),
            TokenWithLocation::new(Token::RETURN, Location{ line: 1, position: 22 }),
            TokenWithLocation::new(Token::TRUE, Location{ line: 1, position: 29 }),
            TokenWithLocation::new(Token::FALSE, Location{ line: 1, position: 34 }),
        ]);
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(tokens, expected_tokens)
    }

    #[test]
    fn parse_strings() {
        let input = String::from("let my_string = \"hello\" + \"world\";");
        let expected_tokens = Ok(vec![
            TokenWithLocation::new(Token::LET, Location::default()),
            TokenWithLocation::new(Token::IDENTIFIER("my_string".to_string()), Location { line: 1, position: 5 }),
            TokenWithLocation::new(Token::EQUAL, Location { line: 1, position: 15 }),
            TokenWithLocation::new(Token::STRING("hello".to_string()), Location { line: 1, position: 17 }),
            TokenWithLocation::new(Token::PLUS, Location { line: 1, position: 25 }),
            TokenWithLocation::new(Token::STRING("world".to_string()), Location { line: 1, position: 27 }),
            TokenWithLocation::new(Token::SEMICOLON, Location { line: 1, position: 34 }),
        ]);
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(tokens, expected_tokens)
    }

    #[test]
    fn parse_everything() {
        let input = String::from("fn plus_1(foo) {\n\tlet number = foo + 1; number\n}");
        let expected_tokens = Ok(vec![
            TokenWithLocation::new(Token::FN, Location::default()),
            TokenWithLocation::new(Token::IDENTIFIER(String::from("plus_1")), Location { line: 1, position: 4 }),
            TokenWithLocation::new(Token::LPAREN, Location { line: 1, position: 10 }),
            TokenWithLocation::new(Token::IDENTIFIER(String::from("foo")), Location { line: 1, position: 11 }),
            TokenWithLocation::new(Token::RPAREN, Location { line: 1, position: 14 }),
            TokenWithLocation::new(Token::LSQUIG, Location { line: 1, position: 16 }),
            TokenWithLocation::new(Token::LET, Location { line: 2, position: 2 }),
            TokenWithLocation::new(Token::IDENTIFIER(String::from("number")), Location { line: 2, position: 6 }),
            TokenWithLocation::new(Token::EQUAL, Location { line: 2, position: 13 }),
            TokenWithLocation::new(Token::IDENTIFIER(String::from("foo")), Location { line: 2, position: 15 }),
            TokenWithLocation::new(Token::PLUS, Location { line: 2, position: 19 }),
            TokenWithLocation::new(Token::INT(1), Location { line: 2, position: 21 }),
            TokenWithLocation::new(Token::SEMICOLON, Location { line: 2, position: 22 }),
            TokenWithLocation::new(Token::IDENTIFIER(String::from("number")), Location { line: 2, position: 24 }),
            TokenWithLocation::new(Token::RSQUIG, Location { line: 3, position: 1 }),
        ]);
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.parse();
        assert_eq!(expected_tokens, tokens);
    }
}
