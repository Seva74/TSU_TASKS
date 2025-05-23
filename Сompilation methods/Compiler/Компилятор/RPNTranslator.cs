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
    public static class RPNTranslator
    {
        static List<Terminal> Input = new List<Terminal>();
        public static List<RPNSymbol> Output = new List<RPNSymbol>();
        public static List<RPNMark> TempMarks = new List<RPNMark>();
        public static List<RPNMark> ConstMarks = new List<RPNMark>();
        static List<RPNSymbol> OperationStack = new List<RPNSymbol>();
        /// <summary>
        /// Возвращает список типа RPNTranslator в формате польской строки из входного списка типа Terminal
        /// </summary>
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
                    OperationStack.Add(new RPNSymbol(ERPNType.F_ConditionalJumpToMark));
                    OperationStack.Add(new RPNMark(ERPNType.М_Mark, EMarkType.WhileBeginMark));
                    TempMarks.Add(new RPNMark(ERPNType.М_Mark, EMarkType.WhileBeginMark));
                    TempMarks.Last().Position = Output.Count;
                    Input.Remove(Input.First());
                }
                //if обрабатывается особым образом
                else if (Input[0].TerminalType == ETerminalType.If)
                {
                    OperationStack.Add(new RPNSymbol(ERPNType.F_ConditionalJumpToMark));
                    OperationStack.Add(new RPNMark(ERPNType.М_Mark,EMarkType.IfMark));
                    TempMarks.Add(new RPNMark(ERPNType.М_Mark, EMarkType.IfMark));
                    Input.Remove(Input.First());
                }
                //else обрабатывается особым образом
                else if (Input[0].TerminalType == ETerminalType.Else)
                {
                    Input.Remove(Input.First());
                }
            }
            //После завершения считывания строки все оставшиеся OperationStack в стеке записываются в Output
            while (OperationStack.Count > 0)
            {
                if (IsWritableInOutput(OperationStack.Last()))
                {
                    Output.Add(OperationStack.Last());
                }
                OperationStack.Remove(OperationStack.Last());
            }
            WriteMarks();
            return Output;
        }
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
        /// <summary>
        /// Возвращает true если input можно записать в OperationStack
        /// </summary>
        public static bool IsWritableInOperationStack(RPNSymbol input)
        {
            if ((input.RPNType == ERPNType.T_Semicolon) || (input.RPNType == ERPNType.T_RightParen) || (input.RPNType == ERPNType.T_RightBracket) || (input.RPNType == ERPNType.T_RightBrace) || (input.RPNType == ERPNType.T_LeftParen))
            {
                return false;
            }
            return true;
        }
        public static bool IsOpeningParenthesis(Terminal input)
        {
            if ((input.TerminalType == ETerminalType.LeftParen) || (input.TerminalType == ETerminalType.LeftBracket) || (input.TerminalType == ETerminalType.LeftBrace))
            {
                return true;
            }
            return false;
        }
        /// <summary>
        /// Возвращает true если input можно записать в Output
        /// </summary>
        public static bool IsWritableInOutput(RPNSymbol input)
        {
            if ((input.RPNType == ERPNType.T_Semicolon) || (input.RPNType == ERPNType.T_RightParen) || (input.RPNType == ERPNType.T_RightBracket) || (input.RPNType == ERPNType.T_RightBrace) || (input.RPNType == ERPNType.T_LeftBrace) || (input.RPNType == ERPNType.T_LeftParen) || (input.RPNType == ERPNType.T_LeftBracket))
            {
                return false;
            }
            return true;
        }
        /// <summary>
        /// Возвращает true если input - операнд
        /// </summary>
        public static bool IsOperand(Terminal input)
        {
            if ((input.TerminalType == ETerminalType.Number) || (input.TerminalType == ETerminalType.TextLine) || (input.TerminalType == ETerminalType.Boolean) || (input.TerminalType == ETerminalType.VariableName))
            {
                return true;
            }
            return false;
        }
        /// <summary>
        /// Возвращает true если input - операция или скобка
        /// </summary>
        public static bool IsOperationOrParenthesis(Terminal input)
        {
            if ((input.TerminalType == ETerminalType.Int) || (input.TerminalType == ETerminalType.String) || (input.TerminalType == ETerminalType.Bool) || (input.TerminalType == ETerminalType.Semicolon) || (input.TerminalType == ETerminalType.Output) || (input.TerminalType == ETerminalType.Input) || (input.TerminalType == ETerminalType.Assignment) || (input.TerminalType == ETerminalType.And) || (input.TerminalType == ETerminalType.Or) || (input.TerminalType == ETerminalType.Equal) || (input.TerminalType == ETerminalType.Less) || (input.TerminalType == ETerminalType.Greater) || (input.TerminalType == ETerminalType.GreaterEqual) || (input.TerminalType == ETerminalType.LessEqual) || (input.TerminalType == ETerminalType.Plus) || (input.TerminalType == ETerminalType.Minus) || (input.TerminalType == ETerminalType.Divide) || (input.TerminalType == ETerminalType.Multiply) || (input.TerminalType == ETerminalType.Modulus) || (input.TerminalType == ETerminalType.Not) ||  (input.TerminalType == ETerminalType.LeftParen) || (input.TerminalType == ETerminalType.RightParen) || (input.TerminalType == ETerminalType.RightBracket) || (input.TerminalType == ETerminalType.LeftBracket) || (input.TerminalType == ETerminalType.RightBrace) || (input.TerminalType == ETerminalType.LeftBrace))
            {
                return true;
            }
            return false;
        }
        /// <summary>
        /// Возвращает true если input - функция инициализации переменной
        /// </summary>
        public static bool IsVariableInitialization(RPNSymbol input)
        {
            if ((input.RPNType == ERPNType.F_Int) || (input.RPNType == ERPNType.F_String) || (input.RPNType == ERPNType.F_Bool))
            {
                return true;
            }
            return false;
        }

        /// <summary>
        /// Переносит оператор input из списка Input в OperatorStack
        /// </summary>
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
                        Output.Add(new RPNSymbol(ERPNType.F_Index));
                        OperationStack.Remove(OperationStack.Last());
                    }
                }

                //Если входная лексема - правая фигурная скобка, то в Output записываются все операции из OperationStack пока там не найдётся левая фигурная скобка
                else if ((OperationStack.Count > 0) && (input.RPNType == ERPNType.T_RightBrace))
                {
                    while ((OperationStack.Count > 0) && (OperationStack.Last().RPNType != ERPNType.T_LeftBrace))
                    {
                        if (IsWritableInOutput(OperationStack.Last()))
                        {
                            Output.Add(OperationStack.Last());
                        }
                        OperationStack.Remove(OperationStack.Last());
                    }
                    //Обработка while
                    if ((TempMarks.Count > 0) && (TempMarks.Last().MarkType == EMarkType.WhileBeginMark))
                    {
                        TempMarks.Add(new RPNMark(ERPNType.М_Mark, EMarkType.WhileEndMark));
                        Output.Add(new RPNMark(ERPNType.М_Mark, EMarkType.WhileEndMark));
                        Output.Add(new RPNSymbol(ERPNType.F_UnconditionalJumpToMark));
                        TempMarks.Last().Position = Output.Count;
                        //TempMarks[TempMarks.Count-2].Position = TempMarks[TempMarks.Count - 2].Position;
                        ConstMarks.Add(TempMarks.Last());
                        ConstMarks.Add(TempMarks[TempMarks.Count-2]);
                        TempMarks.Remove(TempMarks.Last());
                        TempMarks.Remove(TempMarks.Last());
                    }
                    //Обработка if
                    else if ((TempMarks.Count > 0) && (TempMarks.Last().MarkType == EMarkType.IfMark))
                    {
                        TempMarks.Last().Position = Output.Count();
                        ConstMarks.Add(TempMarks.Last());
                        TempMarks.Remove(TempMarks.Last());

                        if ((Input.Count() > 1) && (Input[1].TerminalType == ETerminalType.Else))
                        {
                            ConstMarks.Last().Position += 2;
                            Output.Add(new RPNMark(ERPNType.М_Mark, EMarkType.ElseMark));
                            TempMarks.Add(new RPNMark(ERPNType.М_Mark, EMarkType.ElseMark));
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
                    while ((OperationStack.Count > 0) && (GetRPNSymbolPriority(OperationStack.Last()) > GetRPNSymbolPriority(input)))
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
        /// <summary>
        /// Перевод терминала в символ ОПС
        /// </summary>
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
        /// <summary>
        /// Возвращает приоритет символа ОПС
        /// </summary>
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
            _ => throw new NotImplementedException("КРАШНУТЬСЯ НАФИГ НО ПОНИЖЕ")
        };
        /// <summary>
        /// Перевод функции инициализации одной переменной в функцию инициализации массива этого же типа
        /// </summary>
        public static ERPNType ToArrayInit(RPNSymbol input) => input.RPNType switch
        {
            ERPNType.F_Int => ERPNType.F_IntArray,
            ERPNType.F_String => ERPNType.F_StringArray,
            ERPNType.F_Bool => ERPNType.F_BoolArray,
            _ => throw new NotImplementedException("КРАШНУТЬСЯ НАФИГ НО ЕЩЁ НИЖЕ")
        };
    }
}
