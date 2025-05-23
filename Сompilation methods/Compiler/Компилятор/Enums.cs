namespace Компилятор
{
   public enum ETerminalType
    {
        /// <summary>
        /// Целое число
        /// </summary>
        Number,
        
        /// <summary>
        /// Строка текста
        /// </summary>
        TextLine,
        
        /// <summary>
        /// True или False
        /// </summary>
        Boolean,
        // Operators
        
        /// <summary>
        /// +
        /// </summary>
        Plus,         // +
        
        /// <summary>
        /// -
        /// </summary>
        Minus,        // -
        
        /// <summary>
        /// *
        /// </summary>
        Multiply,     // *
        
        /// <summary>
        /// /
        /// </summary>
        Divide,       // /
        
        /// <summary>
        /// %
        /// </summary>
        Modulus,      // %
        
        /// <summary>
        /// &&
        /// </summary>
        And,          // &&
        
        /// <summary>
        /// ||
        /// </summary>
        Or,           // ||
        
        /// <summary>
        /// !
        /// </summary>
        Not,          // !

        // Parentheses and Brackets
        
        /// <summary>
        /// (
        /// </summary>
        LeftParen,    // (
        
        /// <summary>
        /// )
        /// </summary>
        RightParen,   // )
        
        /// <summary>
        /// [
        /// </summary>
        LeftBracket,  // [
        
        /// <summary>
        /// ]
        /// </summary>
        RightBracket, // ]
        
        /// <summary>
        /// {
        /// </summary>
        LeftBrace,    // {
        
        /// <summary>
        /// }
        /// </summary>
        RightBrace,   // }

        // Quotation mark
        /// <summary>
        /// "
        /// </summary>
        DoubleQuote,  // "

        // Assignment
        /// <summary>
        /// =
        /// </summary>
        Assignment,        // =

        // Identifiers and keywords
       
        /// <summary>
        /// Пользовательская переменная
        /// </summary>
        VariableName,   // a...z, A...Z, _, а...я, А...Я
        
        /// <summary>
        /// if
        /// </summary>
        If,
        
        /// <summary>
        /// else
        /// </summary>
        Else,
        
        /// <summary>
        /// ==
        /// </summary>
        Equal,        // ==
        
        /// <summary>
        /// строго меньше 
        /// </summary>
        Less,         // <
        
        /// <summary>
        /// >
        /// </summary>
        Greater,      // >

        /// <summary>
        /// меньше или равно 
        /// </summary>
        LessEqual,    // <=
        
        /// <summary>
        /// >=
        /// </summary>
        GreaterEqual, // >=
        
        /// <summary>
        /// while
        /// </summary>
        While,
        
        /// <summary>
        /// int
        /// </summary>
        Int,
        
        /// <summary>
        /// string
        /// </summary>
        String,
        
        /// <summary>
        /// bool
        /// </summary>
        Bool,
        
        /// <summary>
        /// Функия вывода данных
        /// </summary>
        Output,
        
        /// <summary>
        /// Функция ввода данных в переменную
        /// </summary>
        Input,
        
        /// <summary>
        /// ;
        /// </summary>
        Semicolon,

        /// <summary>
        /// sqrt function
        /// </summary>
        Sqrt,

        /// <summary>
        /// pow function
        /// </summary>
        Pow

    }

    //RPN = Reverse Polish Notation = Обратная польская нотация = ОПН = ОПС
    public enum ERPNType
    {
        //ОПЕРАЦИИ
        // При описании операций буквами A и B обозначены, соответственно, первый и второй аргументы, используемые в этой операции
        // AB+ == A+B и т.д.

        /// <summary>
        /// Output(A)
        /// </summary>
        F_Output,

        /// <summary>
        /// Input(A)
        /// </summary>
        F_Input,

        /// <summary>
        /// a=b
        /// </summary>
        F_Assignment,    // =

        /// <summary>
        /// A&&B
        /// </summary>
        F_And,          // &&

        /// <summary>
        /// A||B
        /// </summary>
        F_Or,           // ||

        /// <summary>
        /// A==B
        /// </summary>
        F_Equal,        // ==

        /// <summary>
        /// A<B
        /// </summary>
        F_Less,         // <

        /// <summary>
        /// A>B
        /// </summary>
        F_Greater,      // >

        /// <summary>
        /// A<=B
        /// </summary>
        F_LessEqual,    // <=

        /// <summary>
        /// A>=B
        /// </summary>
        F_GreaterEqual, // >=

        /// <summary>
        /// A+B
        /// </summary>
        F_Plus,         // +

        /// <summary>
        /// A-B
        /// </summary>
        F_Minus,        // -

        /// <summary>
        /// A*B
        /// </summary>
        F_Multiply,     // *

        /// <summary>
        /// A/B
        /// </summary>
        F_Divide,       // /

        /// <summary>
        /// A%B
        /// </summary>
        F_Modulus,      // %

        /// <summary>
        /// !A
        /// </summary>
        F_Not,          // !

        /// <summary>
        /// sqrt(A)
        /// </summary>
        F_Sqrt,

        /// <summary>
        /// pow(A,B) A^B
        /// </summary>
        F_Pow,

        /// <summary>
        /// Взятие B-го элемента от массива A
        /// </summary>
        F_Index,      //[]

        /// <summary>
        /// int
        /// </summary>
        F_Int,

        /// <summary>
        /// string
        /// </summary>
        F_String,

        /// <summary>
        /// Создание массива bool именем B числом элементов A
        /// </summary>
        F_Bool,

        /// <summary>
        /// Создание массива int именем B числом элементов A
        /// </summary>
        F_IntArray,

        /// <summary>
        /// Создание массива string именем B числом элементов A
        /// </summary>
        F_StringArray,

        /// <summary>
        /// bool
        /// </summary>
        F_BoolArray,

        //АРГУМЕНТЫ

        /// <summary>
        /// Целое число
        /// </summary>
        A_Number,

        /// <summary>
        /// Строка текста
        /// </summary>
        A_TextLine,

        /// <summary>
        /// True или False
        /// </summary>
        A_Boolean,

        /// <summary>
        /// Пользовательcкая переменная
        /// </summary>
        A_VariableName,   // a...z, A...Z, _, а...я, А...Я

        // Identifiers and keywords

        /// <summary>
        /// if
        /// </summary>
        T_If,

        /// <summary>
        /// else
        /// </summary>
        T_Else,

        /// <summary>
        /// while
        /// </summary>
        T_While,

        /// <summary>
        /// ;
        /// </summary>
        T_Semicolon,

        /// <summary>
        /// (
        /// </summary>
        T_LeftParen,    // (

        /// <summary>
        /// )
        /// </summary>
        T_RightParen,   // )

        /// <summary>
        /// [
        /// </summary>
        T_LeftBracket,  // [

        /// <summary>
        /// ]
        /// </summary>
        T_RightBracket, // ]

        /// <summary>
        /// {
        /// </summary>
        T_LeftBrace,    // {

        /// <summary>
        /// }
        /// </summary>
        T_RightBrace,   // }

        /// <summary>
        /// Если TRUE - выполняется идущий дальше код, ина  че - переход к MarkIf
        /// </summary>
        F_ConditionalJumpToMark,

        /// <summary>
        /// Безусловный переход к MarkElse
        /// </summary>
        F_UnconditionalJumpToMark,

        /// <summary>
        /// Метка-указатель
        /// </summary>
        М_Mark,
    }
    public enum EMarkType
    {
        WhileBeginMark,
        WhileEndMark,
        IfMark,
        ElseMark,
    }
}
