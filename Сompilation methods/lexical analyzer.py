import re
from enum import Enum
from dataclasses import dataclass
from typing import List

# Типы лексем
class TokenType(Enum):
    ID = "ID"               # Идентификатор (x, y, arr)
    NUMBER = "NUMBER"       # Число (5, 3.14)
    ASSIGN = "ASSIGN"       # =
    PLUS = "PLUS"           # +
    MINUS = "MINUS"         # -
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
    COMMA = "COMMA"         # ,
    SQRT = "SQRT"           # sqrt
    SIN = "SIN"             # sin
    COS = "COS"             # cos
    STRING = "STRING"       # "text"
    EOF = "EOF"             # Конец файла

# Класс для представления токена
@dataclass
class Token:
    type: TokenType
    value: str
    line: int
    column: int

# Класс лексического анализатора
class Lexer:
    def __init__(self, source: str):
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
        
        # Список ключевых слов
        self.keywords = {
            "if": TokenType.IF,
            "else": TokenType.ELSE,
            "while": TokenType.WHILE,
            "input": TokenType.INPUT,
            "print": TokenType.PRINT,
            "sqrt": TokenType.SQRT,
            "sin": TokenType.SIN,
            "cos": TokenType.COS
        }

    def error(self, message: str) -> None:
        """Выдать ошибку с указанием строки и столбца."""
        raise Exception(f"Ошибка в строке {self.line}, столбец {self.column}: {message}")

    def peek(self) -> str:
        """Посмотреть следующий символ без продвижения."""
        return self.source[self.pos] if self.pos < len(self.source) else ""

    def advance(self) -> str:
        """Прочитать следующий символ и продвинуть позицию."""
        char = self.peek()
        if char:
            self.pos += 1
            if char == "\n":
                self.line += 1
                self.column = 1
            else:
                self.column += 1
        return char

    def tokenize(self) -> List[Token]:
        """Основная функция для разбиения текста на токены."""
        while self.pos < len(self.source):
            char = self.peek()

            # Пропуск пробелов и переносов строк
            if char.isspace():
                self.advance()
                continue

            # Идентификаторы и ключевые слова
            if char.isalpha():
                self.tokenize_identifier()
                continue

            # Числа
            if char.isdigit():
                self.tokenize_number()
                continue

            # Строки
            if char == '"':
                self.tokenize_string()
                continue

            # Операторы и символы
            if char in "=+-*/(){};[],<>!":
                self.tokenize_operator()
                continue

            # Недопустимый символ
            self.error(f"Недопустимый символ '{char}'")

        # Добавляем токен конца файла
        self.tokens.append(Token(TokenType.EOF, "", self.line, self.column))
        return self.tokens

    def tokenize_identifier(self):
        """Обработать идентификатор или ключевое слово."""
        start_pos = self.pos
        start_column = self.column
        while self.peek().isalnum():
            self.advance()
        identifier = self.source[start_pos:self.pos]
        
        # Проверяем, является ли это ключевым словом
        token_type = self.keywords.get(identifier, TokenType.ID)
        self.tokens.append(Token(token_type, identifier, self.line, start_column))

    def tokenize_number(self):
        """Обработать число (целое или вещественное)."""
        start_pos = self.pos
        start_column = self.column
        while self.peek().isdigit():
            self.advance()
        if self.peek() == ".":
            self.advance()
            while self.peek().isdigit():
                self.advance()
        number = self.source[start_pos:self.pos]
        self.tokens.append(Token(TokenType.NUMBER, number, self.line, start_column))

    def tokenize_string(self):
        """Обработать строковый литерал."""
        self.advance()  # Пропускаем открывающую кавычку
        start_pos = self.pos
        start_column = self.column
        while self.peek() != '"' and self.pos < len(self.source):
            self.advance()
        if self.pos >= len(self.source):
            self.error("Незакрытая строка")
        string = self.source[start_pos:self.pos]
        self.tokens.append(Token(TokenType.STRING, string, self.line, start_column))
        self.advance()  # Пропускаем закрывающую кавычку

    def tokenize_operator(self):
        """Обработать операторы и специальные символы."""
        char = self.advance()
        start_column = self.column - 1

        if char == "=":
            if self.peek() == "=":
                self.advance()
                self.tokens.append(Token(TokenType.EQ, "==", self.line, start_column))
            else:
                self.tokens.append(Token(TokenType.ASSIGN, "=", self.line, start_column))
        elif char == "!":
            if self.peek() == "=":
                self.advance()
                self.tokens.append(Token(TokenType.NEQ, "!=", self.line, start_column))
            else:
                self.error("Ожидался '=' после '!'")
        elif char == "<":
            if self.peek() == "=":
                self.advance()
                self.tokens.append(Token(TokenType.LE, "<=", self.line, start_column))
            else:
                self.tokens.append(Token(TokenType.LT, "<", self.line, start_column))
        elif char == ">":
            if self.peek() == "=":
                self.advance()
                self.tokens.append(Token(TokenType.GE, ">=", self.line, start_column))
            else:
                self.tokens.append(Token(TokenType.GT, ">", self.line, start_column))
        elif char == "+":
            self.tokens.append(Token(TokenType.PLUS, "+", self.line, start_column))
        elif char == "-":
            self.tokens.append(Token(TokenType.MINUS, "-", self.line, start_column))
        elif char == "*":
            self.tokens.append(Token(TokenType.MUL, "*", self.line, start_column))
        elif char == "/":
            self.tokens.append(Token(TokenType.DIV, "/", self.line, start_column))
        elif char == "(":
            self.tokens.append(Token(TokenType.LPAREN, "(", self.line, start_column))
        elif char == ")":
            self.tokens.append(Token(TokenType.RPAREN, ")", self.line, start_column))
        elif char == "{":
            self.tokens.append(Token(TokenType.LBRACE, "{", self.line, start_column))
        elif char == "}":
            self.tokens.append(Token(TokenType.RBRACE, "}", self.line, start_column))
        elif char == ";":
            self.tokens.append(Token(TokenType.SEMICOLON, ";", self.line, start_column))
        elif char == "[":
            self.tokens.append(Token(TokenType.LBRACKET, "[", self.line, start_column))
        elif char == "]":
            self.tokens.append(Token(TokenType.RBRACKET, "]", self.line, start_column))
        elif char == ",":
            self.tokens.append(Token(TokenType.COMMA, ",", self.line, start_column))

# Пример использования
if __name__ == "__main__":
    # Тестовая программа
    source_code = """
    x = 5 + 3.14;
    if (x > 10) {
        print("x is large");
    } else {
        arr[3] = {1, 2, 3};
        print(arr[1]);
    }
    y = sqrt(16);
    """
    
    lexer = Lexer(source_code)
    try:
        tokens = lexer.tokenize()
        for token in tokens:
            print(f"Token(type={token.type}, value='{token.value}', line={token.line}, column={token.column})")
    except Exception as e:
        print(e)