namespace Компилятор
{
    public class Terminal
    {
        /// <summary>
        /// Тип терминала
        /// </summary>
        public ETerminalType TerminalType { get; }
        public int CharPointer { get; set; }
        public int LinePointer { get; set; }
        public Terminal(ETerminalType type, int linePointer, int charPointer)
        {
            TerminalType = type;
            LinePointer = linePointer;
            CharPointer = charPointer;
        }
        public class TextLine : Terminal
        {
            public string Data { get; private set; }
            public TextLine(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.TextLine) throw new ArgumentException("Неверно создан нетерминал");
                Data = data;
            }
        }
        public class Number : Terminal
        {
            public int Data { get; }
            public Number(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.Number) throw new ArgumentException("Неверно создан нетерминал");
                Data = Convert.ToInt32(data);
            }
        }
        public class Boolean : Terminal
        {
            public bool Data { get; private set; }
            public Boolean(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.Boolean) throw new ArgumentException("Неверно создан нетерминал");
                Data = Convert.ToBoolean(data);
            }
        }
        public class Identifier : Terminal
        {
            public string Name { get; }
            public Identifier(ETerminalType type, int linePointer, int charPointer, string data) : base(type, linePointer, charPointer)
            {
                if (type != ETerminalType.VariableName) throw new ArgumentException("Неверно создан нетерминал");
                Name = data;
            }
        }
    }
    

}