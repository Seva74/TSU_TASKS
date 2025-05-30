using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using System.Xml;

namespace Компилятор
{
    /// Статический класс, отвечающий за трансляцию списка терминалов 
    /// (результата синтаксического анализа) в обратную польскую нотацию (RPN).
    public static class RPNTranslator
    {
        /// Входной список терминалов для трансляции.
        static List<Terminal> Input = new List<Terminal>();
        /// Выходной список символов RPN.
        public static List<RPNSymbol> Output = new List<RPNSymbol>();
        /// Список временных меток, используемых для условных переходов и циклов.
        public static List<RPNMark> TempMarks = new List<RPNMark>();
        /// Список постоянных меток (в текущей реализации не используется активно, но может быть полезен для будущих расширений).
        public static List<RPNMark> ConstMarks = new List<RPNMark>();
        /// Стек для хранения операций и скобок в процессе преобразования в RPN (алгоритм Дейкстры).
        static List<RPNSymbol> OperationStack = new List<RPNSymbol>();
        /// Преобразует входной список терминалов в обратную польскую нотацию (RPN).
        /// Реализует алгоритм сортировочной станции (алгоритм Дейкстры).
        /// inputTerminals: Список терминалов для преобразования.
        /// returns: Список символов RPN.
        public static List<RPNSymbol> ConvertToRPN(List<Terminal> inputTerminals)
        {
            // Переинициализация статических списков для каждого вызова
            Input = new List<Terminal>(inputTerminals); // Создаем копию или используем inputTerminals напрямую, если он не будет изменяться извне
            Output = new List<RPNSymbol>();
            TempMarks = new List<RPNMark>();
            ConstMarks = new List<RPNMark>(); // Если используется
            OperationStack = new List<RPNSymbol>();

            while (Input.Count > 0)
            {
                // если любая левая скобка 
                if (IsOpeningParenthesis(Input[0]))
                {
                    OperationStack.Add(TranslateToRPNSymbol(Input[0]));
                    Input.Remove(Input.First());
                }
                //Если операция или скобка - кладём в стек
                else if (IsOperationOrParenthesis(Input[0]))
                {
                    ToStack(TranslateToRPNSymbol(Input[0]));
                    Input.Remove(Input.First());
                }
                //Если операнд - кладём в Output
                else if (IsOperand(Input[0]))
                {
                    Output.Add(TranslateOperand(Input[0]));
                    Input.Remove(Input.First());
                }
                //while обрабатывается особым образом
                else if (Input[0].TerminalType == ETerminalType.While)
                {
                    // Создаем метку начала цикла
                    var whileBeginMark = new RPNMark(ERPNType.М_Mark, EMarkType.WhileBeginMark);
                    TempMarks.Add(whileBeginMark);
                    whileBeginMark.Position = Output.Count;
                    Output.Add(whileBeginMark);
                    
                    // Создаем метку конца цикла
                    var whileEndMark = new RPNMark(ERPNType.М_Mark, EMarkType.WhileEndMark);
                    TempMarks.Add(whileEndMark);
                    
                    // Сначала обрабатываем условие
                    Input.Remove(Input.First()); // Удаляем while
                    
                    // Пропускаем открывающую скобку условия
                    if (Input.Count > 0 && Input[0].TerminalType == ETerminalType.LeftParen)
                    {
                        Input.Remove(Input.First());
                    }
                    
                    // Обрабатываем условие до закрывающей скобки
                    while (Input.Count > 0 && Input[0].TerminalType != ETerminalType.RightParen)
                    {
                        if (IsOperationOrParenthesis(Input[0]))
                        {
                            ToStack(TranslateToRPNSymbol(Input[0]));
                        }
                        else if (IsOperand(Input[0]))
                        {
                            Output.Add(TranslateOperand(Input[0]));
                        }
                        Input.Remove(Input.First());
                    }
                    
                    // Выталкиваем оставшиеся операции из стека
                    while (OperationStack.Count > 0 && OperationStack.Last().RPNType != ERPNType.T_LeftParen)
                    {
                        if (IsWritableInOutput(OperationStack.Last()))
                        {
                            Output.Add(OperationStack.Last());
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                    
                    // Удаляем закрывающую скобку условия
                    if (Input.Count > 0 && Input[0].TerminalType == ETerminalType.RightParen)
                    {
                        Input.Remove(Input.First());
                    }
                    
                    // Добавляем метку конца цикла и условный переход в правильном порядке
                    Output.Add(whileEndMark);
                    Output.Add(new RPNSymbol(ERPNType.F_ConditionalJumpToMark));
                }
                //if обрабатывается особым образом
                else if (Input[0].TerminalType == ETerminalType.If)
                {
                    OperationStack.Add(new RPNSymbol(ERPNType.F_ConditionalJumpToMark));
                    var ifMark = new RPNMark(ERPNType.М_Mark, EMarkType.IfMark);
                    OperationStack.Add(ifMark);
                    TempMarks.Add(ifMark);
                    Input.Remove(Input.First());
                }
                //else обрабатывается особым образом
                else if (Input[0].TerminalType == ETerminalType.Else)
                {
                    var elseMark = new RPNMark(ERPNType.М_Mark, EMarkType.ElseMark);
                    TempMarks.Add(elseMark);
                    Output.Add(elseMark);
                    Output.Add(new RPNSymbol(ERPNType.F_UnconditionalJumpToMark));
                    Input.Remove(Input.First());
                }
                else if (Input[0].TerminalType == ETerminalType.RightBrace)
                {
                    //Если входная лексема - правая фигурная скобка, то в Output записываются все операции из OperationStack пока там не найдётся левая фигурная скобка
                    while ((OperationStack.Count > 0) && (OperationStack.Last().RPNType != ERPNType.T_LeftBrace))
                    {
                        if (IsWritableInOutput(OperationStack.Last()))
                        {
                            Output.Add(OperationStack.Last());
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                    
                    //Если левая фигурная скобка - условие If или While, то в Output записываются соответствующие символы
                    if (OperationStack.Count > 0)
                    {
                        OperationStack.Remove(OperationStack.Last());
                    }
                    
                    // Проверяем, есть ли незавершенный цикл while
                    var whileBeginMark = TempMarks.LastOrDefault(m => m.MarkType == EMarkType.WhileBeginMark);
                    var whileEndMark = TempMarks.LastOrDefault(m => m.MarkType == EMarkType.WhileEndMark && m.Position == null);
                    
                    if (whileBeginMark != null && whileEndMark != null)
                    {
                        // Добавляем безусловный переход к началу цикла
                        Output.Add(whileBeginMark);
                        Output.Add(new RPNSymbol(ERPNType.F_UnconditionalJumpToMark));
                        
                        // Устанавливаем позицию метки конца цикла
                        whileEndMark.Position = Output.Count;
                        
                        // Удаляем обработанные метки
                        TempMarks.Remove(whileBeginMark);
                        TempMarks.Remove(whileEndMark);
                    }
                    
                    Input.Remove(Input.First());
                }
            }
            //После завершения считывания строки все оставшиеся OperationStack в стеке записываются в Output
            while (OperationStack.Count > 0)
            {
                if (IsWritableInOutput(OperationStack.Last()))
                {
                    Output.Add(OperationStack.Last());
                    
                    // Если это метка начала цикла while, добавляем безусловный переход
                    if (OperationStack.Last() is RPNMark mark && mark.MarkType == EMarkType.WhileBeginMark)
                    {
                        // Добавляем безусловный переход к началу цикла
                        Output.Add(TempMarks.FirstOrDefault(m => m.MarkType == EMarkType.WhileBeginMark && m.Position == null));
                        Output.Add(new RPNSymbol(ERPNType.F_UnconditionalJumpToMark));
                        
                        // Устанавливаем позицию метки конца цикла
                        var endMark = TempMarks.FirstOrDefault(m => m.MarkType == EMarkType.WhileEndMark && m.Position == null);
                        if (endMark != null)
                        {
                            endMark.Position = Output.Count;
                        }
                    }
                }
                OperationStack.Remove(OperationStack.Last());
            }
            WriteMarks();
            return Output;
        }
        /// Транслирует терминал-операнд (число, строка, булево, идентификатор) в соответствующий символ RPN.
        /// input: Входной терминал.
        /// returns: Символ RPN, представляющий операнд.
        public static RPNSymbol TranslateOperand(Terminal input)
        {
            if (input is Terminal.TextLine)
            {
                var output = new RPNTextLine(ERPNType.A_TextLine);
                var inp = input as Terminal.TextLine;
                output.CharPointer = inp.CharPointer;
                output.LinePointer = inp.LinePointer;
                output.Data = inp.Data;
                return output;
            }
            if (input is Terminal.Boolean)
            {
                var output = new RPNBoolean(ERPNType.A_Boolean);
                var inp = input as Terminal.Boolean;
                output.CharPointer = inp.CharPointer;
                output.LinePointer = inp.LinePointer;
                output.Data = inp.Data;
                return output;
            }
            if (input is Terminal.Number)
            {
                var output = new RPNNumber(ERPNType.A_Number);
                var inp = input as Terminal.Number;
                output.CharPointer = inp.CharPointer;
                output.LinePointer = inp.LinePointer;
                output.Data = inp.Data;
                return output;
            }
            if (input is Terminal.Identifier)
            {
                var output = new RPNIdentifier(ERPNType.A_VariableName);
                var inp = input as Terminal.Identifier;
                output.CharPointer = inp.CharPointer;
                output.LinePointer = inp.LinePointer;
                output.Name = inp.Name;
                return output;
            }
            //заглушка чтобы он не ругался
            var ou = new RPNSymbol(ERPNType.A_VariableName);
            return ou;
        }
        /// Проставляет значения позиций для меток в выходной RPN-строке.
        /// В текущей реализации используется для `ConstMarks`, но может быть расширено.
        public static void WriteMarks()
        {
            for (int i = 0; i < Output.Count; i++)
            {
                if (Output[i] is RPNMark)
                {
                    if (ConstMarks.Count > 0)
                    {
                        var ou = Output[i] as RPNMark;
                        ou.Position = ConstMarks[0].Position;
                        ConstMarks.Remove(ConstMarks[0]);
                    }
                    else
                    {
                        break;
                    }
                }
            }
        }
        /// Проверяет, можно ли данный символ RPN записать в стек операций `OperationStack`.
        /// Скобки и некоторые другие служебные символы не записываются напрямую.
        /// input: Символ RPN для проверки.
        /// returns: True, если символ можно записать в стек операций.
        public static bool IsWritableInOperationStack(RPNSymbol input)
        {
            if ((input.RPNType == ERPNType.T_Semicolon) || (input.RPNType == ERPNType.T_RightParen) || (input.RPNType == ERPNType.T_RightBracket) || (input.RPNType == ERPNType.T_RightBrace) || (input.RPNType == ERPNType.T_LeftParen))
            {
                return false;
            }
            return true;
        }
        /// Проверяет, является ли входной терминал открывающей скобкой ({, (, [).
        /// input: Входной терминал.
        /// returns: True, если терминал является открывающей скобкой.
        public static bool IsOpeningParenthesis(Terminal input)
        {
            if ((input.TerminalType == ETerminalType.LeftParen) || (input.TerminalType == ETerminalType.LeftBracket) || (input.TerminalType == ETerminalType.LeftBrace))
            {
                return true;
            }
            return false;
        }
        /// Проверяет, можно ли данный символ RPN записать в выходную строку `Output`.
        /// Скобки и некоторые другие служебные символы не записываются в выходную строку напрямую.
        /// input: Символ RPN для проверки.
        /// returns: True, если символ можно записать в выходную строку.
        public static bool IsWritableInOutput(RPNSymbol input)
        {
            if ((input.RPNType == ERPNType.T_Semicolon) || (input.RPNType == ERPNType.T_RightParen) || (input.RPNType == ERPNType.T_RightBracket) || (input.RPNType == ERPNType.T_RightBrace) || (input.RPNType == ERPNType.T_LeftBrace) || (input.RPNType == ERPNType.T_LeftParen) || (input.RPNType == ERPNType.T_LeftBracket))
            {
                return false;
            }
            return true;
        }
        /// Проверяет, является ли входной терминал операндом (число, строка, булево, имя переменной).
        /// input: Входной терминал.
        /// returns: True, если терминал является операндом.
        public static bool IsOperand(Terminal input)
        {
            if ((input.TerminalType == ETerminalType.Number) || (input.TerminalType == ETerminalType.TextLine) || (input.TerminalType == ETerminalType.Boolean) || (input.TerminalType == ETerminalType.VariableName))
            {
                return true;
            }
            return false;
        }
        /// Проверяет, является ли входной терминал операцией или скобкой.
        /// input: Входной терминал.
        /// returns: True, если терминал является операцией или скобкой.
        public static bool IsOperationOrParenthesis(Terminal input)
        {
            if ((input.TerminalType == ETerminalType.Int) || (input.TerminalType == ETerminalType.String) || (input.TerminalType == ETerminalType.Bool) || (input.TerminalType == ETerminalType.Semicolon) || (input.TerminalType == ETerminalType.Output) || (input.TerminalType == ETerminalType.Input) || (input.TerminalType == ETerminalType.Assignment) || (input.TerminalType == ETerminalType.And) || (input.TerminalType == ETerminalType.Or) || (input.TerminalType == ETerminalType.Equal) || (input.TerminalType == ETerminalType.Less) || (input.TerminalType == ETerminalType.Greater) || (input.TerminalType == ETerminalType.GreaterEqual) || (input.TerminalType == ETerminalType.LessEqual) || (input.TerminalType == ETerminalType.Plus) || (input.TerminalType == ETerminalType.Minus) || (input.TerminalType == ETerminalType.Divide) || (input.TerminalType == ETerminalType.Multiply) || (input.TerminalType == ETerminalType.Modulus) || (input.TerminalType == ETerminalType.Not) ||  (input.TerminalType == ETerminalType.LeftParen) || (input.TerminalType == ETerminalType.RightParen) || (input.TerminalType == ETerminalType.RightBracket) || (input.TerminalType == ETerminalType.LeftBracket) || (input.TerminalType == ETerminalType.RightBrace) || (input.TerminalType == ETerminalType.LeftBrace))
            {
                return true;
            }
            return false;
        }
        /// Проверяет, является ли входной символ RPN операцией инициализации переменной (int, string, bool).
        /// input: Символ RPN для проверки.
        /// returns: True, если символ является операцией инициализации переменной.
        public static bool IsVariableInitialization(RPNSymbol input)
        {
            if ((input.RPNType == ERPNType.F_Int) || (input.RPNType == ERPNType.F_String) || (input.RPNType == ERPNType.F_Bool))
            {
                return true;
            }
            return false;
        }

        /// Помещает входной символ RPN (операцию или скобку) в стек операций `OperationStack`,
        /// согласно правилам алгоритма сортировочной станции (учитывая приоритеты операций).
        /// input: Символ RPN для помещения в стек.
        public static void ToStack(RPNSymbol input)
        {
            if (OperationStack.Count > 0)
            {
                //Если входная лексема - правая круглая скобка, то в Output записываются все операции из OperationStack пока там не найдётся левая круглая скобка
                if ((OperationStack.Count > 0) && (input.RPNType == ERPNType.T_RightParen))
                {
                    while ((OperationStack.Count > 0) && (OperationStack.Last().RPNType != ERPNType.T_LeftParen))
                    {
                        if (IsWritableInOutput(OperationStack.Last()))
                        {
                            Output.Add(OperationStack.Last());
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                    //Если левая круглая скобка - условие If или While, то в Output записываются соответствующие символы
                    if (OperationStack.Count > 1)
                    {
                        OperationStack.Remove(OperationStack.Last());
                        if (OperationStack.Last().RPNType == ERPNType.М_Mark)
                        {
                            if (IsWritableInOutput(OperationStack.Last()))
                                Output.Add(OperationStack.Last());
                            OperationStack.Remove(OperationStack.Last());
                            if (IsWritableInOutput(OperationStack.Last()))
                                Output.Add(OperationStack.Last());
                            OperationStack.Remove(OperationStack.Last());
                        }
                    }
                }
                else if ((OperationStack.Count > 0) && (input.RPNType == ERPNType.T_RightBracket))
                {
                    //Если входная лексема - правая квадратная скобка, то в Output записываются все операции из OperationStack пока там не найдётся левая квадратная скобка
                    while ((OperationStack.Count > 0) && (OperationStack.Last().RPNType != ERPNType.T_LeftBracket))
                    {
                        if (IsWritableInOutput(OperationStack.Last()))
                        {
                            Output.Add(OperationStack.Last());
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                    //Если перед левой квадратной скобкой стоит операция инициализации переменной - в Output записывается операция инициализации массива переменных такого типа
                    if ((OperationStack.Count > 1) && IsVariableInitialization(OperationStack[OperationStack.Count-2]))
                    {
                        if (OperationStack.Last().RPNType == ERPNType.T_LeftBracket)
                        {
                            OperationStack.Remove(OperationStack.Last());
                        }
                        if (IsVariableInitialization(OperationStack.Last()))
                        {
                            Output.Add(TranslateOperand(Input[1]));
                            Input.Remove(Input[1]);
                            Output.Add(new RPNSymbol(ToArrayInit(OperationStack.Last())));
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                    //Иначе - в Output записывается операция индексации
                    else if (OperationStack.Count > 0)
                    {
                        if (OperationStack.Last().RPNType == ERPNType.T_LeftBracket)
                        {
                            OperationStack.Remove(OperationStack.Last());
                        }
                        Output.Add(new RPNSymbol(ERPNType.F_Index));
                    }
                }
                else if ((OperationStack.Count > 0) && (input.RPNType == ERPNType.T_RightBrace))
                {
                    //Если входная лексема - правая фигурная скобка, то в Output записываются все операции из OperationStack пока там не найдётся левая фигурная скобка
                    while ((OperationStack.Count > 0) && (OperationStack.Last().RPNType != ERPNType.T_LeftBrace))
                    {
                        if (IsWritableInOutput(OperationStack.Last()))
                        {
                            Output.Add(OperationStack.Last());
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                    //Если левая фигурная скобка - условие If или While, то в Output записываются соответствующие символы
                    if (OperationStack.Count > 0)
                    {
                        OperationStack.Remove(OperationStack.Last());
                    }
                    if ((TempMarks.Count > 0) && (TempMarks.Last().MarkType == EMarkType.WhileBeginMark))
                    {
                        // Добавляем безусловный переход к началу цикла
                        Output.Add(TempMarks.Last());
                        Output.Add(new RPNSymbol(ERPNType.F_UnconditionalJumpToMark));
                        TempMarks.Last().Position = Output.Count();
                        ConstMarks.Add(TempMarks.Last());
                        TempMarks.Remove(TempMarks.Last());
                    }
                    else if ((TempMarks.Count > 0) && (TempMarks.Last().MarkType == EMarkType.IfMark))
                    {
                        TempMarks.Last().Position = Output.Count();
                        ConstMarks.Add(TempMarks.Last());
                        TempMarks.Remove(TempMarks.Last());

                        if ((Input.Count() > 1) && (Input[1].TerminalType == ETerminalType.Else))
                        {
                            var elseMark = new RPNMark(ERPNType.М_Mark, EMarkType.ElseMark);
                            Output.Add(elseMark);
                            TempMarks.Add(elseMark);
                            Output.Add(new RPNSymbol(ERPNType.F_UnconditionalJumpToMark));
                        }
                    }
                    else if ((TempMarks.Count > 0) && (TempMarks.Last().MarkType == EMarkType.ElseMark))
                    {
                        TempMarks.Last().Position = Output.Count();
                        ConstMarks.Add(TempMarks.Last());
                        TempMarks.Remove(TempMarks.Last());
                    }
                }
                else
                {
                    // Пока в стеке есть операции с приоритетом больше или равным текущей,
                    // выталкиваем их в выходную строку
                    while (OperationStack.Count > 0 && 
                           OperationStack.Last().RPNType != ERPNType.T_LeftParen && 
                           OperationStack.Last().RPNType != ERPNType.T_LeftBracket && 
                           OperationStack.Last().RPNType != ERPNType.T_LeftBrace &&
                           GetRPNSymbolPriority(OperationStack.Last()) >= GetRPNSymbolPriority(input))
                    {
                        if (IsWritableInOutput(OperationStack.Last()))
                        {
                            Output.Add(OperationStack.Last());
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                }
            }
            if (IsWritableInOperationStack(input))
            {
                OperationStack.Add(input);
            }
        }
        /// Перевод терминала в символ ОПС
        public static RPNSymbol TranslateToRPNSymbol(Terminal input) => input.TerminalType switch
        {
            ETerminalType.Assignment => new RPNSymbol(ERPNType.F_Assignment),
            ETerminalType.And => new RPNSymbol(ERPNType.F_And),
            ETerminalType.Or => new RPNSymbol(ERPNType.F_Or),
            ETerminalType.Equal => new RPNSymbol(ERPNType.F_Equal),
            ETerminalType.Less => new RPNSymbol(ERPNType.F_Less),
            ETerminalType.Greater => new RPNSymbol(ERPNType.F_Greater),
            ETerminalType.LessEqual => new RPNSymbol(ERPNType.F_LessEqual),
            ETerminalType.GreaterEqual => new RPNSymbol(ERPNType.F_GreaterEqual),
            ETerminalType.Plus => new RPNSymbol(ERPNType.F_Plus),
            ETerminalType.Minus => new RPNSymbol(ERPNType.F_Minus),
            ETerminalType.Multiply => new RPNSymbol(ERPNType.F_Multiply),
            ETerminalType.Divide => new RPNSymbol(ERPNType.F_Divide),
            ETerminalType.Modulus => new RPNSymbol(ERPNType.F_Modulus),
            ETerminalType.Not => new RPNSymbol(ERPNType.F_Not),
            ETerminalType.Int => new RPNSymbol(ERPNType.F_Int),
            ETerminalType.String => new RPNSymbol(ERPNType.F_String),
            ETerminalType.Bool => new RPNSymbol(ERPNType.F_Bool),
            ETerminalType.Input => new RPNSymbol(ERPNType.F_Input),
            ETerminalType.Output => new RPNSymbol(ERPNType.F_Output),
            ETerminalType.LeftBracket => new RPNSymbol(ERPNType.T_LeftBracket),
            ETerminalType.RightBracket => new RPNSymbol(ERPNType.T_RightBracket),
            ETerminalType.LeftParen => new RPNSymbol(ERPNType.T_LeftParen),
            ETerminalType.RightParen => new RPNSymbol(ERPNType.T_RightParen),
            ETerminalType.LeftBrace => new RPNSymbol(ERPNType.T_LeftBrace),
            ETerminalType.RightBrace => new RPNSymbol(ERPNType.T_RightBrace),
            ETerminalType.VariableName => new RPNSymbol(ERPNType.A_VariableName),
            ETerminalType.Number => new RPNSymbol(ERPNType.A_Number),
            ETerminalType.TextLine => new RPNSymbol(ERPNType.A_TextLine),
            ETerminalType.Boolean => new RPNSymbol(ERPNType.A_Boolean),
            ETerminalType.Semicolon => new RPNSymbol(ERPNType.T_Semicolon),

            //ETerminalType.If => new RPNSymbol(ERPNType.ConditionalJumpToMark),
            //ETerminalType.Else => new RPNSymbol(ERPNType.UnconditionalJumpToMark),
            //ETerminalType.While => new RPNSymbol(ERPNType.ConditionalJumpToMark),
            _ => throw new NotImplementedException("КРАШНУТЬСЯ НАФИГ")
        };
        /// Возвращает приоритет символа ОПС
        public static int GetRPNSymbolPriority(RPNSymbol input) => input.RPNType switch
        {
            ERPNType.T_Semicolon => -1,
            ERPNType.T_LeftParen => -1,
            ERPNType.T_LeftBrace => -1,
            ERPNType.T_LeftBracket => -1,
            ERPNType.F_ConditionalJumpToMark => -1,
            ERPNType.F_UnconditionalJumpToMark => -1,
            ERPNType.М_Mark => -1,
            ERPNType.F_Assignment => 0,
            ERPNType.F_And => 1,
            ERPNType.F_Or => 1,
            ERPNType.F_Equal => 2,
            ERPNType.F_Less => 2,
            ERPNType.F_Greater => 2,
            ERPNType.F_LessEqual => 2,
            ERPNType.F_GreaterEqual => 2,
            ERPNType.F_Plus => 3,
            ERPNType.F_Minus => 3,
            ERPNType.F_Multiply => 4,
            ERPNType.F_Divide => 4,
            ERPNType.F_Modulus => 4,
            ERPNType.F_Not => 5,
            ERPNType.F_Int => 6,
            ERPNType.F_String => 6,
            ERPNType.F_Bool => 6,
            ERPNType.F_IntArray => 6,
            ERPNType.F_StringArray => 6,
            ERPNType.F_BoolArray => 6,
            ERPNType.F_Input => 7,
            ERPNType.F_Output => 7,
            ERPNType.F_Index => 8,
            _ => throw new NotImplementedException("КРАШНУТЬСЯ ПОНИЖЕ")
        };
        /// Перевод функции инициализации одной переменной в функцию инициализации массива этого же типа
        public static ERPNType ToArrayInit(RPNSymbol input) => input.RPNType switch
        {
            ERPNType.F_Int => ERPNType.F_IntArray,
            ERPNType.F_String => ERPNType.F_StringArray,
            ERPNType.F_Bool => ERPNType.F_BoolArray,
            _ => throw new NotImplementedException("КРАШНУТЬСЯ НИЖЕ")
        };
    }
}
