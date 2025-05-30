using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Компилятор
{
    // Базовый класс для всех символов, используемых в обратной польской нотации (RPN).
    public class RPNSymbol
    {
        // Тип символа RPN (например, операция, операнд, метка).
        public ERPNType RPNType { get; set; }
        // Конструктор для инициализации символа RPN с указанным типом.
        // type: Тип символа RPN.
        public RPNSymbol(ERPNType type)
        {
            RPNType = type;
        }
        // Номер символа в строке исходного кода, откуда был порожден этот RPN символ (для отладки).
        public int CharPointer { get; set; }
        // Номер строки в исходном коде, откуда был порожден этот RPN символ (для отладки).
        public int LinePointer { get; set; }
    }
    // Представляет метку в RPN, используемую для условных и безусловных переходов.
    public class RPNMark : RPNSymbol
    {
        // Конструктор для инициализации метки RPN.
        // type: Тип символа RPN (должен быть М_Mark).
        // markType: Конкретный тип метки (например, начало цикла, метка if).
        public RPNMark(ERPNType type, EMarkType markType)
            : base(type) // Вызов конструктора базового класса
        {
            RPNType = type;
            MarkType = markType;
        }
        // Указывает, является ли метка финальной (используется для разрешения некоторых переходов в RPNTranslator).
        public bool IsFinal { get; set; } = false;
        // Конкретный тип метки (например, WhileBeginMark, IfMark).
        public EMarkType MarkType { get; set; }
        // Позиция (индекс) в списке RPN инструкций, на которую указывает эта метка.
        // Может быть null, если позиция еще не определена.
        public int? Position { get; set; }
    }
    // Представляет строковый литерал в RPN.
    public class RPNTextLine : RPNSymbol
    {
        // Конструктор для строкового литерала RPN.
        // type: Тип символа RPN (должен быть A_TextLine).
        public RPNTextLine(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        // Строковое значение литерала.
        public string Data { get; set; }
    }
    // Представляет числовой литерал (целое число) в RPN.
    public class RPNNumber : RPNSymbol
    {
        // Конструктор для числового литерала RPN.
        // type: Тип символа RPN (должен быть A_Number).
        public RPNNumber(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        // Целочисленное значение литерала.
        public int Data { get; set; }
    }
    // Представляет булевый литерал (true/false) в RPN.
    public class RPNBoolean : RPNSymbol
    {
        // Конструктор для булевого литерала RPN.
        // type: Тип символа RPN (должен быть A_Boolean).
        public RPNBoolean(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        // Булево значение литерала.
        public bool Data { get; set; }
    }
    // Представляет идентификатор (имя переменной) в RPN.
    public class RPNIdentifier : RPNSymbol
    {
        // Конструктор для идентификатора RPN.
        // type: Тип символа RPN (должен быть A_VariableName).
        public RPNIdentifier(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        // Имя идентификатора (переменной).
        public string Name { get; set; }
    }
}
