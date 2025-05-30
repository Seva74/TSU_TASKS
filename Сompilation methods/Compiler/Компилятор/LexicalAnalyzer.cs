using System.Globalization;

namespace Компилятор
{
    /// Статический класс, отвечающий за лексический анализ исходного кода.
    /// Разбивает входную строку на последовательность токенов (терминалов).
    public static class LexicalAnalyzer
    {
        /// Входная строка кода для анализа.
        private static string Data { get; set; }
        /// Список терминалов (токенов), полученных в результате анализа.
        private static List<Terminal> Terminals { get; } = [];

        /// Указатель на текущий символ в строке (относительно текущей строки).
        private static int _charPointer = 1;
        /// Указатель на текущую строку.
        private static int _linePointer = 1;
        /// Сохраненное значение указателя на символ в строке для текущего терминала.
        private static int _char = 1;

        /// Указатель на текущий символ в общей входной строке `Data`.
        private static int _pointer = 0;
        /// Свойство для доступа и инкрементации указателя `_pointer`.
        /// При переходе на новую строку обновляет `_linePointer` и сбрасывает `_charPointer`.
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
        /// Возвращает текущий символ из входной строки `Data` по указателю `Pointer`.
        private static char CurrentChar
        {
            get
            {
                return Data[Pointer];
            }
        }
        /// Выполняет лексический анализ входной строки кода.
        /// data: Строка исходного кода.
        /// returns: True, если лексический анализ прошел успешно, иначе выбрасывает исключение.
        public static bool IsLexicalCorrect(string data)
        {
            Data = data;
            _pointer = 0;
            _charPointer = 1;
            _linePointer = 1;
            _char = 1;
            Terminals.Clear(); // Очистка списка терминалов перед новым анализом

            while (Pointer < data.Length)
            {
                Start_Analyse(); // Запуск анализа для текущего символа
            }
            return true;
        }
        
        /// Возвращает список терминалов, полученных в результате лексического анализа.
        /// returns: Список терминалов.
        public static List<Terminal> GetTerminals()
        {
            return Terminals;
        }

        /// Пропускает пробельные символы, инкрементируя указатель.
        internal static void SkipWhitespace()
        {
            Pointer++;
        }

        /// Обрабатывает простой токен (односимвольный или ключевое слово без значения) и добавляет его в список терминалов.
        /// terminalType: Тип терминала.
        internal static void ProcessSimpleToken(ETerminalType terminalType)
        {
            ReadTerminal(terminalType);
            Pointer++;
        }

        /// Добавляет терминал указанного типа в список `Terminals`.
        /// Используется для терминалов, не имеющих дополнительного значения (например, операторы, скобки).
        /// terminalType: Тип добавляемого терминала.
        private static void ReadTerminal(ETerminalType terminalType)
        {
            Terminals.Add(new Terminal(terminalType, _linePointer, _char));
        }

        /// Добавляет терминал указанного типа со значением в список `Terminals`.
        /// Используется для чисел, строк, булевых значений и идентификаторов.
        /// terminalType: Тип добавляемого терминала.
        /// value: Значение терминала.
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

        /// Определяет группу текущего символа (цифра, буква, кавычка, пробел, оператор и т.д.).
        /// Эта информация используется таблицей переходов для определения следующего действия анализатора.
        /// returns: Строковое представление группы символов (например, "<ц>" для цифры).
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
        
        /// Основной метод, запускающий анализ текущего символа на основе таблицы переходов.
        /// Сохраняет текущую позицию символа перед анализом.
        private static void Start_Analyse()
        {
            _char = _charPointer; // Сохраняем позицию символа в строке для текущего токена
            string charGroup = CurentCharGroup();
            
            // Пытаемся получить действие из таблицы переходов для текущей группы символов
            if (TransitionTable.TryGetAction(charGroup, out var action))
            {
                action(); // Выполняем действие (метод анализа конкретного типа токена)
            }
            else
            {
                Console.WriteLine($"Некорректный символ: {CurrentChar}" +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                throw new Exception("Недопустимый символ.");
            }
        }
        
        /// Анализирует последовательность цифр для формирования числового токена.
        internal static void NUM_Analyse()
        {
            string number = string.Empty;
            do
            {
                number += CurrentChar;
                Pointer++;
            }
            while (Pointer < Data.Length && CurentCharGroup() == "<ц>"); // Продолжаем, пока текущий символ - цифра и не вышли за пределы строки
            ReadTerminal(ETerminalType.Number, number);
        }

        /// Анализирует последовательность букв и цифр для формирования идентификатора или ключевого слова.
        internal static void ID_Analyse()
        {
            string identifier = string.Empty;
            do
            {
                identifier += CurrentChar;
                Pointer++;
            }
            while (Pointer < Data.Length && (CurentCharGroup() == "<ц>" || CurentCharGroup() == "<б>")); // Продолжаем, пока текущий символ - буква или цифра и не вышли за пределы строки
            
            // Проверка, является ли полученный идентификатор ключевым словом
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
                default:
                    ReadTerminal(ETerminalType.VariableName, identifier);
                    break;
            }
        }

        /// Анализирует строковый литерал, заключенный в двойные кавычки.
        internal static void STR_Analyse()
        {
            string textLine = string.Empty;
            Pointer++; // Пропускаем открывающую кавычку
            while (Pointer < Data.Length && CurrentChar != '"') // Читаем символы до закрывающей кавычки
            {
                textLine += CurrentChar;
                Pointer++;
            }
            if (Pointer < Data.Length && CurrentChar == '"') // Если нашли закрывающую кавычку
            {
                Pointer++; // Пропускаем закрывающую кавычку
                ReadTerminal(ETerminalType.TextLine, textLine);
            }
            else
            {
                // Ошибка: не найдена закрывающая кавычка
                Console.WriteLine($"Незакрытая строка: " +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                throw new Exception("Незакрытая строка.");
            }
        }

        /// Анализирует оператор '<' или '<='.
        internal static void LESS_Analyse()
        {
            Pointer++; // Пропускаем первый символ '<'
            if (Pointer < Data.Length && CurrentChar == '=') // Проверяем следующий символ на равенство
            {
                ReadTerminal(ETerminalType.LessEqual);
                Pointer++;
            }
            else
            {
                ReadTerminal(ETerminalType.Less);
            }
        }

        /// Анализирует оператор '>' или '>='.
        internal static void MORE_Analyse()
        {
            Pointer++; // Пропускаем первый символ '>'
            if (Pointer < Data.Length && CurrentChar == '=') // Проверяем следующий символ на равенство
            {
                ReadTerminal(ETerminalType.GreaterEqual);
                Pointer++;
            }
            else
            {
                ReadTerminal(ETerminalType.Greater);
            }
        }

        /// Анализирует оператор '=' или '=='.
        internal static void EQUAL_Analyse()
        {
            Pointer++; // Пропускаем первый символ '='
            if (Pointer < Data.Length && CurrentChar == '=') // Проверяем следующий символ на равенство
            {
                ReadTerminal(ETerminalType.Equal);
                Pointer++;
            }
            else
            {
                ReadTerminal(ETerminalType.Assignment);
            }
        }

        /// Анализирует оператор '&&'.
        internal static void AND_Analyse()
        {
            Pointer++; // Пропускаем первый символ '&'
            if (Pointer < Data.Length && CurrentChar == '&') // Проверяем следующий символ на '&'
            {
                ReadTerminal(ETerminalType.And);
                Pointer++;
            }
            else
            {
                // Ошибка: одиночный '&' не поддерживается
                Console.WriteLine($"Некорректный оператор: одиночный '&'" +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                throw new Exception("Некорректный оператор: одиночный '&'. Используйте '&&'.");
            }
        }

        /// Анализирует оператор '||'.
        internal static void OR_Analyse()
        {
            Pointer++; // Пропускаем первый символ '|'
            if (Pointer < Data.Length && CurrentChar == '|') // Проверяем следующий символ на '|'
            {
                ReadTerminal(ETerminalType.Or);
                Pointer++;
            }
            else
            {
                // Ошибка: одиночный '|' не поддерживается
                Console.WriteLine($"Некорректный оператор: одиночный '|'" +
                    $"\tСтрока {_linePointer};" +
                    $"\tСимвол {_charPointer};");
                throw new Exception("Некорректный оператор: одиночный '|'. Используйте '||'.");
            }
        }
    }
}
