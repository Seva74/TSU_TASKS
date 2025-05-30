namespace Компилятор
{
    /// Базовый класс для всех терминалов (токенов), получаемых лексическим анализатором.
    public class Terminal
    {
        /// Тип терминала (например, число, оператор, ключевое слово).
        public ETerminalType TerminalType { get; }
        /// Номер символа в строке исходного кода, где начинается этот терминал.
        public int CharPointer { get; set; }
        /// Номер строки в исходном коде, где начинается этот терминал.
        public int LinePointer { get; set; }
        /// Конструктор для инициализации базового терминала.
        /// type: Тип терминала.
        /// linePointer: Номер строки.
        /// charPointer: Номер символа в строке.
        public Terminal(ETerminalType type, int linePointer, int charPointer)
        {
            TerminalType = type;
            LinePointer = linePointer;
            CharPointer = charPointer;
        }
        /// Представляет терминал типа "строковый литерал".
        public class TextLine : Terminal
        {
            /// Строковое значение литерала (без кавычек).
            public string Data { get; private set; }
            /// Конструктор для терминала "строковый литерал".
            public TextLine(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.TextLine) throw new ArgumentException("Неверный тип терминала для TextLine. Ожидался ETerminalType.TextLine.");
                Data = data;
            }
        }
        /// Представляет терминал типа "числовой литерал" (целое число).
        public class Number : Terminal
        {
            /// Целочисленное значение литерала.
            public int Data { get; }
            /// Конструктор для терминала "числовой литерал".
            public Number(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.Number) throw new ArgumentException("Неверный тип терминала для Number. Ожидался ETerminalType.Number.");
                Data = Convert.ToInt32(data);
            }
        }
        /// Представляет терминал типа "булевый литерал" (true/false).
        public class Boolean : Terminal
        {
            /// Булево значение литерала.
            public bool Data { get; private set; }
            /// Конструктор для терминала "булевый литерал".
            public Boolean(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.Boolean) throw new ArgumentException("Неверный тип терминала для Boolean. Ожидался ETerminalType.Boolean.");
                Data = Convert.ToBoolean(data);
            }
        }
        /// Представляет терминал типа "идентификатор" (имя переменной).
        public class Identifier : Terminal
        {
            /// Имя идентификатора.
            public string Name { get; }
            /// Конструктор для терминала "идентификатор".
            public Identifier(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.VariableName) throw new ArgumentException("Неверный тип терминала для Identifier. Ожидался ETerminalType.VariableName.");
                Name = data;
            }
        }
    }
    

}