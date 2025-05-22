# Syntax analyzer and POLIZ generator for SimpleCalc language in Python
from enum import Enum
from dataclasses import dataclass
from typing import List

# Token types from lexical analyzer
class TokenType(Enum):
    ID = "ID"               # Identifier (x, y, arr)
    NUMBER = "NUMBER"       # Number (5, 3.14)
    ASSIGN = "ASSIGN"       # =
    PLUS = "PLUS"           # +
    MINUS = "MINUS"         # - (binary or unary)
    MUL = "MUL"             # *
    DIV = "DIV"             # /
    LPAREN = "LPAREN"       # (
    RPAREN = "RPAREN"       # )
    SEMICOLON = "SEMICOLON" # ;
    LBRACE = "LBRACE"       # {
    RBRACE = "RBRACE"       # }
    IF = "IF"               # if
    ELSE = "ELSE"           # else
    WHILE = "WHILE"         # while
    INPUT = "INPUT"         # input
    PRINT = "PRINT"         # print
    EQ = "EQ"               # ==
    NEQ = "NEQ"             # !=
    LT = "LT"               # <
    GT = "GT"               # >
    LE = "LE"               # <=
    GE = "GE"               # >=
    LBRACKET = "LBRACKET"   # [
    RBRACKET = "RBRACKET"   # ]
    SQRT = "SQRT"           # sqrt
    SIN = "SIN"             # sin
    COS = "COS"             # cos
    STRING = "STRING"       # "text"
    EOF = "EOF"             # End of file

# Token class
@dataclass
class Token:
    type: TokenType
    value: str
    line: int
    column: int

class Parser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0
        self.poliz = []
        self.labels = {}
        self.label_count = 0

    def current_token(self) -> Token:
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def advance(self):
        self.pos += 1

    def new_label(self):
        self.label_count += 1
        return f"L{self.label_count}"

    def expect(self, token_type: TokenType) -> Token:
        token = self.current_token()
        if token and token.type == token_type:
            self.advance()
            return token
        raise SyntaxError(f"Expected {token_type.value}, got {token.type.value if token else 'EOF'} at line {token.line if token else 'unknown'}")

    # <Program> ::= <Statements>
    def parse_program(self):
        self.parse_statements()
        self.poliz.append("HALT")

    # <Statements> ::= <Statement> | <Statement> <Statements>
    def parse_statements(self):
        while self.pos < len(self.tokens) and self.current_token().type != TokenType.EOF:
            self.parse_statement()

    # <Statement> ::= <Assignment> ";" | <If> | <While> | <Input> ";" | <Output> ";"
    def parse_statement(self):
        token = self.current_token()
        if token.type == TokenType.IF:
            self.parse_if()
        elif token.type == TokenType.WHILE:
            self.parse_while()
        elif token.type == TokenType.INPUT:
            self.parse_input()
            self.expect(TokenType.SEMICOLON)
        elif token.type == TokenType.PRINT:
            self.parse_output()
            self.expect(TokenType.SEMICOLON)
        else:
            self.parse_assignment()
            self.expect(TokenType.SEMICOLON)

    # <Assignment> ::= ID "=" <Expression> | ID "[" <Expression> "]" "=" <Expression>
    def parse_assignment(self):
        ident = self.expect(TokenType.ID)
        token = self.current_token()
        if token.type == TokenType.LBRACKET:
            self.advance()
            self.parse_expression()
            self.expect(TokenType.RBRACKET)
            self.expect(TokenType.ASSIGN)
            self.parse_expression()
            self.poliz.append(ident.value)
            self.poliz.append("ARR_ASSIGN")
        else:
            self.expect(TokenType.ASSIGN)
            self.parse_expression()
            self.poliz.append(ident.value)
            self.poliz.append("=")

    # <If> ::= "if" "(" <Expression> ")" "{" <Statements> "}" | "if" "(" <Expression> ")" "{" <Statements> "}" "else" "{" <Statements> "}"
    def parse_if(self):
        self.expect(TokenType.IF)
        self.expect(TokenType.LPAREN)
        self.parse_expression()
        self.expect(TokenType.RPAREN)
        label_else = self.new_label()
        label_end = self.new_label()
        self.poliz.append(label_else)
        self.poliz.append("JZ")
        self.expect(TokenType.LBRACE)
        self.parse_statements()
        self.expect(TokenType.RBRACE)
        token = self.current_token()
        if token and token.type == TokenType.ELSE:
            self.advance()
            self.poliz.append(label_end)
            self.poliz.append("JMP")
            self.labels[label_else] = len(self.poliz)
            self.expect(TokenType.LBRACE)
            self.parse_statements()
            self.expect(TokenType.RBRACE)
            self.labels[label_end] = len(self.poliz)
        else:
            self.labels[label_else] = len(self.poliz)

    # <While> ::= "while" "(" <Expression> ")" "{" <Statements> "}"
    def parse_while(self):
        self.expect(TokenType.WHILE)
        label_start = self.new_label()
        label_end = self.new_label()
        self.labels[label_start] = len(self.poliz)
        self.expect(TokenType.LPAREN)
        self.parse_expression()
        self.expect(TokenType.RPAREN)
        self.poliz.append(label_end)
        self.poliz.append("JZ")
        self.expect(TokenType.LBRACE)
        self.parse_statements()
        self.expect(TokenType.RBRACE)
        self.poliz.append(label_start)
        self.poliz.append("JMP")
        self.labels[label_end] = len(self.poliz)

    # <Input> ::= "input" "(" ID ")"
    def parse_input(self):
        self.expect(TokenType.INPUT)
        self.expect(TokenType.LPAREN)
        ident = self.expect(TokenType.ID)
        self.poliz.append(ident.value)
        self.poliz.append("INPUT")
        self.expect(TokenType.RPAREN)

    # <Output> ::= "print" "(" <Expression> ")" | "print" "(" STRING ")"
    def parse_output(self):
        self.expect(TokenType.PRINT)
        self.expect(TokenType.LPAREN)
        token = self.current_token()
        if token.type == TokenType.STRING:
            self.poliz.append(token.value)
            self.advance()
        else:
            self.parse_expression()
        self.poliz.append("PRINT")
        self.expect(TokenType.RPAREN)

    # <Expression> ::= <Additive>
    def parse_expression(self):
        self.parse_additive()

    # <Additive> ::= <Multiplicative> | <Multiplicative> ("+" | "-") <Additive>
    def parse_additive(self):
        self.parse_multiplicative()
        token = self.current_token()
        if token and token.type in [TokenType.PLUS, TokenType.MINUS]:
            op = token.type
            self.advance()
            self.parse_additive()
            self.poliz.append("+" if op == TokenType.PLUS else "-")

    # <Multiplicative> ::= <Unary> | <Unary> ("*" | "/") <Multiplicative>
    def parse_multiplicative(self):
        self.parse_unary()
        token = self.current_token()
        if token and token.type in [TokenType.MUL, TokenType.DIV]:
            op = token.type
            self.advance()
            self.parse_multiplicative()
            self.poliz.append("*" if op == TokenType.MUL else "/")

    # <Unary> ::= "-" <Primary> | "sqrt" "(" <Expression> ")" | "sin" "(" <Expression> ")" | "cos" "(" <Expression> ")" | <Primary>
    def parse_unary(self):
        token = self.current_token()
        if token.type == TokenType.MINUS:
            self.advance()
            self.parse_primary()
            self.poliz.append("UMINUS")
        elif token.type in [TokenType.SQRT, TokenType.SIN, TokenType.COS]:
            func = token.type
            self.advance()
            self.expect(TokenType.LPAREN)
            self.parse_expression()
            self.expect(TokenType.RPAREN)
            self.poliz.append(func.value.lower())
        else:
            self.parse_primary()

    # <Primary> ::= NUMBER | STRING | ID | ID "[" <Expression> "]" | "(" <Expression> ")"
    def parse_primary(self):
        token = self.current_token()
        if token.type == TokenType.NUMBER:
            self.poliz.append(token.value)
            self.advance()
        elif token.type == TokenType.STRING:
            self.poliz.append(token.value)
            self.advance()
        elif token.type == TokenType.ID:
            ident = token.value
            self.advance()
            token = self.current_token()
            if token and token.type == TokenType.LBRACKET:
                self.advance()
                self.parse_expression()
                self.expect(TokenType.RBRACKET)
                self.poliz.append(ident)
                self.poliz.append("ARR_GET")
            else:
                self.poliz.append(ident)
        elif token.type == TokenType.LPAREN:
            self.advance()
            self.parse_expression()
            self.expect(TokenType.RPAREN)
        else:
            raise SyntaxError(f"Unexpected token {token.type.value} at line {token.line}")

# Example usage with lexical analyzer integration
def parse_simplecalc(code):
    from lexer import Lexer  # Assuming lexer.py exists with the lexical analyzer
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    parser.parse_program()
    return parser.poliz

if __name__ == "__main__":
    # Test code
    code = """
    x = 5 + 3.14;
    if (x > 10) {
        print("x is large");
    } else {
        arr[3] = 42;
        print(arr[1]);
    }
    y = sqrt(16);
    """
    try:
        poliz = parse_simplecalc(code)
        print("Generated POLIZ:", poliz)
    except SyntaxError as e:
        print(f"Syntax Error: {e}")