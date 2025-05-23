using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Компилятор
{
    public class RPNSymbol
    {
        public ERPNType RPNType { get; set; }
        public RPNSymbol(ERPNType type)
        {
            RPNType = type;
        }
        public int CharPointer { get; set; }
        public int LinePointer { get; set; }
    }
    public class RPNMark : RPNSymbol
    {
        public RPNMark(ERPNType type, EMarkType markType)
            : base(type)
        {
            RPNType = type;
            MarkType = markType;
        }
        public bool IsFinal { get; set; } = false;
        public EMarkType MarkType { get; set; }
        public int? Position { get; set; }
    }
    public class RPNTextLine : RPNSymbol
    {
        public RPNTextLine(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        public string Data { get; set; }
    }
    public class RPNNumber : RPNSymbol
    {
        public RPNNumber(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        public int Data { get; set; }
    }
    public class RPNBoolean : RPNSymbol
    {
        public RPNBoolean(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        public bool Data { get; set; }
    }
    public class RPNIdentifier : RPNSymbol
    {
        public RPNIdentifier(ERPNType type)
            : base(type)
        {
            RPNType = type;
        }
        public string Name { get; set; }
    }
}
