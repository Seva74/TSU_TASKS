using System.Globalization;

namespace Компилятор
{
    public static class LexicalAnalyzer
    {
        private static string Data { get; set; }
        private static List<Terminal> Terminals { get; } = [];

        private static int _charPointer = 1;
        private static int _linePointer = 1;
        private static int _char = 1;

        private static int _pointer = 0;
        private static int Pointer
        {
            get
            {
                return _pointer;
            }
            set
            {
                _pointer++;
                _charPointer++;
                if(Data.ElementAtOrDefault(_pointer) == '\n')
                {
                    _linePointer++;
                    _charPointer = 0;
                    _char = 1;
                }
            }
        }
        private static char CurrentChar
        {
            get
            {
                return Data[Pointer];
            }
        }
        public static bool IsLexicalCorrect(string data)
        {
            Data = data;


          
            while (Pointer < data.Length)
            {
                Start_Analyse();
            }
            return true;
        }
        public static List<Terminal> GetTerminals()
        {
            return Terminals;
        }
        private static void ReadTerminal(ETerminalType terminalType)
        {
            Terminals.Add(new Terminal(terminalType, _linePointer, _char));
        }
        private static void ReadTerminal(ETerminalType terminalType, string value)
        {
            
            switch (terminalType)
            {
                case ETerminalType.Number:
                    Terminals.Add(new Terminal.Number(terminalType, _linePointer, _char, value));
                    break;
                case ETerminalType.TextLine:
                    Terminals.Add(new Terminal.TextLine(terminalType, _linePointer, _char, value));
                    break;
                case ETerminalType.Boolean:
                    Terminals.Add(new Terminal.Boolean(terminalType, _linePointer, _char, value));
                    break;
                case ETerminalType.VariableName:
                    Terminals.Add(new Terminal.Identifier(terminalType, _linePointer, _char, value));
                    break;
                default:
                    throw new NotImplementedException("Невозможный тип терминала");
            }
        }
        private static string CurentCharGroup()
        {
            if (CurrentChar >= '0' && CurrentChar <= '9')
                return "<ц>";

            else if (CurrentChar >= 'a' && CurrentChar <= 'z' ||
                     CurrentChar >= 'A' && CurrentChar <= 'Z' ||
                     CurrentChar == '_')
                return "<б>";

            else if (CurrentChar == '\"') return "<\">";
            else if (CurrentChar == ' ') return "< >";
            else if (CurrentChar == '\n') return "< >";
            else if (CurrentChar == ';') return "<;>";

            else if (CurrentChar == '+') return "<+>";
            else if (CurrentChar == '-') return "<->";
            else if (CurrentChar == '*') return "<*>";
            else if (CurrentChar == '/') return "</>";
            else if (CurrentChar == '%') return "<%>";

            else if (CurrentChar == '<') return "<<>";
            else if (CurrentChar == '>') return "<>>";
            else if (CurrentChar == '=') return "<=>";
            else if (CurrentChar == '&') return "<&>";
            else if (CurrentChar == '|') return "<|>";
            else if (CurrentChar == '!') return "<!>";
            else if (CurrentChar == '(') return "<(>";
            else if (CurrentChar == ')') return "<)>";
            else if (CurrentChar == '[') return "<[>";
            else if (CurrentChar == ']') return "<]>";
            else if (CurrentChar == '{') return "<{>";
            else if (CurrentChar == '}') return "<}>";

            else if (CurrentChar >= 'а' && CurrentChar <= 'я' ||
                     CurrentChar >= 'А' && CurrentChar <= 'Я' ||
                     CurrentChar == '?' ||
                     CurrentChar == ',' ||
                     CurrentChar == '.')
                return "<o>";

            else
            {
                Console.WriteLine($"Некорректный символ: {CurrentChar}" +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                throw new ArgumentOutOfRangeException("символ \"" + CurrentChar + "\" недопустим в грамматике");
            }
        }
        
        private static void Start_Analyse()
        {
            _char = _charPointer;
            switch (CurentCharGroup())
            {
                case "<ц>":
                    NUM_Analyse();
                    break;

                case "<б>":
                    ID_Analyse();
                    break;

                case "< >":
                    Pointer++;
                    break;

                case "<\">":
                    STR_Analyse();
                    break;

                case "<;>":
                    ReadTerminal(ETerminalType.Semicolon);
                    Pointer++;
                    break;

                case "<+>":
                    ReadTerminal(ETerminalType.Plus);
                    Pointer++;
                    break;

                case "<->":
                    ReadTerminal(ETerminalType.Minus);
                    Pointer++;
                    break;

                case "<*>":
                    ReadTerminal(ETerminalType.Multiply);
                    Pointer++;
                    break;

                case "</>":
                    ReadTerminal(ETerminalType.Divide);
                    Pointer++;
                    break;

                case "<%>":
                    ReadTerminal(ETerminalType.Modulus);
                    Pointer++;
                    break;

                case "<<>":
                    LESS_Analyse();
                    break;

                case "<>>":
                    MORE_Analyse();
                    break;

                case "<=>":
                    EQUAL_Analyse();
                    break;

                case "<&>":
                    AND_Analyse();
                    break;

                case "<|>":
                    OR_Analyse();
                    break;

                case "<!>":
                    ReadTerminal(ETerminalType.Not);
                    Pointer++;
                    break;

                case "<(>":
                    ReadTerminal(ETerminalType.LeftParen);
                    Pointer++;
                    break;

                case "<)>":
                    ReadTerminal(ETerminalType.RightParen);
                    Pointer++;
                    break;

                case "<[>":
                    ReadTerminal(ETerminalType.LeftBracket);
                    Pointer++;
                    break;

                case "<]>":
                    ReadTerminal(ETerminalType.RightBracket);
                    Pointer++;
                    break;

                case "<{>":
                    ReadTerminal(ETerminalType.LeftBrace);
                    Pointer++;
                    break;

                case "<}>":
                    ReadTerminal(ETerminalType.RightBrace);
                    Pointer++;
                    break;

                default:
                    Console.WriteLine($"Некорректный сомвол: {CurrentChar}" +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                    throw new Exception("Недопустимый символ.");
            }
        }
        private static void NUM_Analyse()
        {
            string number = string.Empty;
            do
            {
                number += CurrentChar;
                Pointer++;
            }
            while (CurentCharGroup() == "<ц>");
            ReadTerminal(ETerminalType.Number, number);
        }
        private static void ID_Analyse()
        {
            string identifier = string.Empty;
            do
            {
                identifier += CurrentChar;
                Pointer++;
            }
            while (CurentCharGroup() == "<ц>" || CurentCharGroup() == "<б>");
            
            switch (identifier)
            {
                case "while":
                    ReadTerminal(ETerminalType.While);
                    break;
                case "if":
                    ReadTerminal(ETerminalType.If);
                    break;
                case "else":
                    ReadTerminal(ETerminalType.Else);
                    break;
                case "int":
                    ReadTerminal(ETerminalType.Int);
                    break;
                case "string":
                    ReadTerminal(ETerminalType.String);
                    break;
                case "bool":
                    ReadTerminal(ETerminalType.Bool);
                    break;
                case "output":
                    ReadTerminal(ETerminalType.Output);
                    break;
                case "input":
                    ReadTerminal(ETerminalType.Input);
                    break;
                case "true":
                case "false":
                    ReadTerminal(ETerminalType.Boolean, identifier);
                    break;
                case "sqrt":
                    ReadTerminal(ETerminalType.Sqrt);
                    break;
                case "pow":
                    ReadTerminal(ETerminalType.Pow);
                    break;
                default:
                    ReadTerminal(ETerminalType.VariableName, identifier);
                    break;
            }

        }
        private static void STR_Analyse()
        {
            Pointer++;
            string textLine = string.Empty;
            do
            {
                textLine += CurrentChar;
                Pointer++;
            }
            while (CurentCharGroup() != "<\">");
            Pointer++;
            ReadTerminal(ETerminalType.TextLine, textLine);
        }
        private static void LESS_Analyse()
        {
            if (Data[Pointer+1] == '=')
            {
                ReadTerminal(ETerminalType.LessEqual);
                Pointer++;
            }
            else
            {
                ReadTerminal(ETerminalType.Less);
            }
            Pointer++;
        }
        private static void MORE_Analyse()
        {
            if (Data[Pointer + 1] == '=')
            {
                ReadTerminal(ETerminalType.GreaterEqual);
                Pointer++;
            }
            else
            {
                ReadTerminal(ETerminalType.Greater);
            }
            Pointer++;
        }
        private static void EQUAL_Analyse()
        {
            if (Data[Pointer + 1] == '=')
            {
                ReadTerminal(ETerminalType.Equal);
                Pointer++;
            }
            else
            {
                ReadTerminal(ETerminalType.Assignment);
            }
            Pointer++;
        }
        private static void AND_Analyse()
        {
            if (Data[Pointer + 1] == '&')
            {
                ReadTerminal(ETerminalType.And);
                Pointer++;
                Pointer++;
            }
            else
            {
                Console.WriteLine($"Некорректный символ: {CurrentChar}" +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                throw new NotImplementedException();
            }
        }
        private static void OR_Analyse()
        {
            if (Data[Pointer + 1] == '|')
            {
                ReadTerminal(ETerminalType.And);
                Pointer++;
                Pointer++;
            }
            else
            {
                Console.WriteLine($"Некорректный символ: {CurrentChar}" +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                throw new NotImplementedException();
            }
        }
    }
}
