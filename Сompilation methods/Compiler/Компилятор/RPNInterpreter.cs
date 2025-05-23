using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Компилятор
{
    public class RPNInterpreter
    {
        public static void ExecuteInstructions(List<RPNSymbol> rpn)
        {
            int currentLineNumber = 0;
            int markedLineNumber = -1;
            var variables = new Dictionary<string, List<string>>();
            var stack = new Stack<RPNSymbol>();
            int iteration = 0;

            for (; iteration < rpn.Count; iteration++)
            {
                RPNSymbol symbol = rpn[iteration];
                if (currentLineNumber >= markedLineNumber)
                {
                    ERPNType type = symbol.RPNType;

                    if (type == ERPNType.A_VariableName ||
                        type == ERPNType.A_Number ||
                        type == ERPNType.A_TextLine ||
                        type == ERPNType.A_Boolean ||
                        type == ERPNType.М_Mark)
                    {
                        stack.Push(symbol);
                    }
                    else if (symbol.RPNType == ERPNType.F_Int)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception("Переменная уже объявлена");
                            }

                            variables.Add(var.Name, new List<string> { $"{ERPNType.A_Number}", $"" });

                            Console.WriteLine($"int {var.Name}");
                        }
                        else
                        {
                            throw new Exception("После типа 'int' ожидался идентификатор переменной");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_String)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception("Переменная уже объявлена");
                            }

                            variables.Add(var.Name, new List<string> { $"{ERPNType.A_TextLine}", $"" });

                            Console.WriteLine($"string {var.Name}");
                        }
                        else
                        {
                            throw new Exception("После типа 'string' ожидался идентификатор переменной");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Bool)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception("Переменная уже объявлена");
                            }

                            variables.Add(var.Name, new List<string> { $"{ERPNType.A_Boolean}", $"" });

                            Console.WriteLine($"bool {var.Name}");
                        }
                        else
                        {
                            throw new Exception("После типа 'bool' ожидался идентификатор переменной");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_IntArray)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception("Переменная уже объявлена");
                            }

                            if (stack.Peek() is RPNNumber number)
                            {
                                stack.Pop();
                                variables.Add(var.Name, new List<string> { $"{ERPNType.A_Number}", $"{number.Data}" });
                                for (int i = 0; i < number.Data; i++)
                                {
                                    variables[var.Name].Add("");
                                }
                                Console.WriteLine($"int[] {var.Name} = {number.Data}");
                            }
                            else
                            {
                                throw new Exception("После типа 'int[]' ожидалось число элементов массива");
                            }
                        }
                        else
                        {
                            throw new Exception("После типа 'int[]' ожидался идентификатор массива");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_StringArray)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception("Переменная уже объявлена");
                            }

                            if (stack.Peek() is RPNNumber number)
                            {
                                stack.Pop();
                                variables.Add(var.Name, new List<string> { $"{ERPNType.A_TextLine}", $"{number.Data}" });
                                for (int i = 0; i < number.Data; i++)
                                {
                                    variables[var.Name].Add("");
                                }
                                Console.WriteLine($"string[] {var.Name} = {number.Data}");
                            }
                            else
                            {
                                throw new Exception("После типа 'string[]' ожидалось число элементов массива");
                            }
                        }
                        else
                        {
                            throw new Exception("После типа 'string[]' ожидался идентификатор массива");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_BoolArray)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception("Переменная уже объявлена");
                            }

                            if (stack.Peek() is RPNNumber number)
                            {
                                stack.Pop();
                                variables.Add(var.Name, new List<string> { $"{ERPNType.A_Boolean}", $"{number.Data}" });
                                for (int i = 0; i < number.Data; i++)
                                {
                                    variables[var.Name].Add("");
                                }
                                Console.WriteLine($"bool[] {var.Name} = {number.Data}");
                            }
                            else
                            {
                                throw new Exception("После типа 'bool[]' ожидалось число элементов массива");
                            }
                        }
                        else
                        {
                            throw new Exception("После типа 'bool[]' ожидался идентификатор массива");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Assignment)
                    {
                        if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;
                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] != variables[var2][0])
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                variables[var1][1] = variables[var2][1];
                                Console.WriteLine($"{variables[var1][0]} {var1} = {variables[var1][1]}");
                            }
                            else throw new Exception("Неверный тип аргументов");
                        }
                        else if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();
                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                variables[var][1] = number;
                                Console.WriteLine($"{variables[var][0]} {var} = {variables[var][1]}");
                            }
                            else
                            {
                                RPNSymbol rPNSymbol = stack.Pop();
                                if (rPNSymbol.RPNType == ERPNType.F_Index)
                                {
                                    string index = (stack.Pop() as RPNNumber).Data.ToString();
                                    string var = (stack.Pop() as RPNIdentifier).Name;

                                    if (!variables.ContainsKey(var))
                                    {
                                        throw new Exception("Обращение к несуществующей переменной!");
                                    }

                                    variables[var][int.Parse(index)+2] = number;
                                }
                                else throw new Exception("Неверный тип аргументов");

                            }
                        }
                        else if (stack.Peek() is RPNTextLine)
                        {
                            string text = (stack.Pop() as RPNTextLine).Data;
                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_TextLine.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                variables[var][1] = text;
                                Console.WriteLine($"{variables[var][0]} {var} = {variables[var][1]}");
                            }
                            else
                            {
                                RPNSymbol rPNSymbol = stack.Pop();
                                if (rPNSymbol.RPNType == ERPNType.F_Index)
                                {
                                    string index = (stack.Pop() as RPNNumber).Data.ToString();
                                    string var = (stack.Pop() as RPNIdentifier).Name;

                                    if (!variables.ContainsKey(var))
                                    {
                                        throw new Exception("Обращение к несуществующей переменной!");
                                    }

                                    variables[var][int.Parse(index) + 2] = text;
                                }
                                else throw new Exception("Неверный тип аргументов");

                            }
                        }
                        else if (stack.Peek() is RPNBoolean)
                        {
                            bool boolean = (stack.Pop() as RPNBoolean).Data;
                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Boolean.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                variables[var][1] = boolean.ToString();
                                Console.WriteLine($"{variables[var][0]} {var} = {variables[var][1]}");
                            }
                            else
                            {
                                RPNSymbol rPNSymbol = stack.Pop();
                                if (rPNSymbol.RPNType == ERPNType.F_Index)
                                {
                                    string index = (stack.Pop() as RPNNumber).Data.ToString();
                                    string var = (stack.Pop() as RPNIdentifier).Name;

                                    if (!variables.ContainsKey(var))
                                    {
                                        throw new Exception("Обращение к несуществующей переменной!");
                                    }

                                    variables[var][int.Parse(index) + 2] = boolean.ToString();
                                }
                                else throw new Exception("Неверный тип аргументов");

                            }
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Greater)
                    {
                        Console.ReadLine();
                        if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();
                            string var = (stack.Pop() as RPNIdentifier).Name;

                            if (!variables.ContainsKey(var))
                            {
                                throw new Exception("Обращение к несуществующей переменной!");
                            }

                            if (variables[var][0] != ERPNType.A_Number.ToString())
                            {
                                throw new Exception("Несоответствие типов");
                            }

                            RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                            boolResult.Data = int.Parse(variables[var][1]) > int.Parse(number);

                            stack.Push(boolResult);
                        }
                        else if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;
                            string var1 = (stack.Pop() as RPNIdentifier).Name;

                            if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                            {
                                throw new Exception("Обращение к несуществующей переменной!");
                            }

                            if (variables[var1][0] != ERPNType.A_Number.ToString() || variables[var2][0] != ERPNType.A_Number.ToString())
                            {
                                throw new Exception("Несоответствие типов");
                            }

                            RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                            boolResult.Data = int.Parse(variables[var1][1]) > int.Parse(variables[var2][1]);

                            stack.Push(boolResult);
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Less)
                    {
                        if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();
                            string var = (stack.Pop() as RPNIdentifier).Name;

                            if (!variables.ContainsKey(var))
                            {
                                throw new Exception("Обращение к несуществующей переменной!");
                            }

                            if (variables[var][0] != ERPNType.A_Number.ToString())
                            {
                                throw new Exception("Несоответствие типов");
                            }

                            RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                            boolResult.Data = int.Parse(variables[var][1]) < int.Parse(number);

                            stack.Push(boolResult);
                        }
                        else if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;
                            string var1 = (stack.Pop() as RPNIdentifier).Name;

                            if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                            {
                                throw new Exception("Обращение к несуществующей переменной!");
                            }

                            if (variables[var1][0] != ERPNType.A_Number.ToString() || variables[var2][0] != ERPNType.A_Number.ToString())
                            {
                                throw new Exception("Несоответствие типов");
                            }

                            RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                            boolResult.Data = int.Parse(variables[var1][1]) < int.Parse(variables[var2][1]);

                            stack.Push(boolResult);
                        }
                        else
                        {
                            throw new Exception("Неверный тип аргументов");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Equal)
                    {
                        if (stack.Peek() is RPNNumber)
                        {
                            string var2 = (stack.Pop() as RPNNumber).Data.ToString();
                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                                boolResult.Data = variables[var1][1] == var2;
                                stack.Push(boolResult);
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string var1 = (stack.Pop() as RPNNumber).Data.ToString();

                                RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                                boolResult.Data = var1 == var2;

                                stack.Push(boolResult);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNTextLine)
                        {
                            string var2 = (stack.Pop() as RPNTextLine).Data;
                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] != ERPNType.A_TextLine.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                                boolResult.Data = variables[var1][1] == var2;

                                stack.Push(boolResult);
                            }
                            else if (stack.Peek() is RPNTextLine)
                            {
                                string var1 = (stack.Pop() as RPNTextLine).Data;

                                RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                                boolResult.Data = var1 == var2;

                                stack.Push(boolResult);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else throw new Exception("Неверный тип аргументов");
                    }
                    else if (symbol.RPNType == ERPNType.F_And)
                    {
                        if (stack.Peek() is RPNBoolean)
                        {
                            bool bool2 = (stack.Pop() as RPNBoolean).Data;
                            if (stack.Peek() is RPNBoolean)
                            {
                                bool bool1 = (stack.Pop() as RPNBoolean).Data;

                                RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                                boolResult.Data = bool1 && bool2;

                                stack.Push(boolResult);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else
                        {
                            throw new Exception("Неверный тип аргументов");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Or)
                    {
                        if (stack.Peek() is RPNBoolean)
                        {
                            bool bool2 = (stack.Pop() as RPNBoolean).Data;
                            if (stack.Peek() is RPNBoolean)
                            {
                                bool bool1 = (stack.Pop() as RPNBoolean).Data;

                                RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                                boolResult.Data = bool1 || bool2;

                                stack.Push(boolResult);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else
                        {
                            throw new Exception("Неверный тип аргументов");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Not)
                    {
                        if (stack.Peek() is RPNBoolean)
                        {
                            bool bool1 = (stack.Pop() as RPNBoolean).Data;

                            RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean);

                            boolResult.Data = !bool1;

                            stack.Push(boolResult);
                        }
                        else
                        {
                            throw new Exception("Неверный тип аргументов");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_ConditionalJumpToMark)
                    {
                        RPNMark mark = stack.Pop() as RPNMark;

                        if (stack.Pop() is RPNBoolean boolResult)
                        {
                            if (!boolResult.Data)
                            {
                                iteration = mark.Position.Value - 1;
                            }
                        }
                        else { throw new Exception("Неверный тип аргументов условного оператора"); }
                    }
                    else if (symbol.RPNType == ERPNType.F_UnconditionalJumpToMark)
                    {
                        RPNMark mark = stack.Pop() as RPNMark;

                        iteration = mark.Position.Value - 1;
                    }
                    else if (symbol.RPNType == ERPNType.F_Plus)
                    {
                        if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] == ERPNType.A_Number.ToString())
                                {
                                    RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                    rPNNumber.Data = int.Parse(variables[var1][1]) + int.Parse(variables[var2][1]);
                                    stack.Push(rPNNumber);
                                }
                                else if (variables[var1][0] == ERPNType.A_TextLine.ToString())
                                {
                                    RPNTextLine rPNTextLine = new RPNTextLine(ERPNType.A_TextLine);
                                    rPNTextLine.Data = variables[var1][1] + variables[var2][1];
                                    stack.Push(rPNTextLine);
                                }
                                else
                                {
                                    throw new Exception("Неверный тип аргументов");
                                }
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number = (stack.Pop() as RPNNumber).Data.ToString();

                                if (!variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var2][1]) + int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else if (stack.Peek() is RPNTextLine)
                            {
                                string text = (stack.Pop() as RPNTextLine).Data;

                                if (!variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var2][0] != ERPNType.A_TextLine.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNTextLine rPNTextLine = new RPNTextLine(ERPNType.A_TextLine);
                                rPNTextLine.Data = variables[var2][1] + text;
                                stack.Push(rPNTextLine);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var][1]) + int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number2 = (stack.Pop() as RPNNumber).Data.ToString();

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(number) + int.Parse(number2);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNTextLine)
                        {
                            string text = (stack.Pop() as RPNTextLine).Data;

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_TextLine.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNTextLine rPNTextLine = new RPNTextLine(ERPNType.A_TextLine);
                                rPNTextLine.Data = text + variables[var][1];
                                stack.Push(rPNTextLine);
                            }
                            else if (stack.Peek() is RPNTextLine)
                            {
                                string text2 = (stack.Pop() as RPNTextLine).Data;

                                RPNTextLine rPNTextLine = new RPNTextLine(ERPNType.A_TextLine);
                                rPNTextLine.Data = text2 + text;
                                stack.Push(rPNTextLine);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else
                        {
                            throw new Exception("Неверный тип аргументов");
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Minus)
                    {
                        if (stack.Count == 2)
                        {
                            if (stack.Peek() is RPNNumber)
                            {
                                string number = (stack.Pop() as RPNNumber).Data.ToString();
                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = -int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = -int.Parse(variables[var][1]);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] != ERPNType.A_Number.ToString() || variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                if (variables[var1][0] == ERPNType.A_Number.ToString())
                                {
                                    RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                    rPNNumber.Data = int.Parse(variables[var1][1]) - int.Parse(variables[var2][1]);
                                    stack.Push(rPNNumber);
                                }
                                else
                                {
                                    throw new Exception("Неверный тип аргументов");
                                }
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number = (stack.Pop() as RPNNumber).Data.ToString();

                                if (!variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var2][1]) - int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var][1]) - int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number2 = (stack.Pop() as RPNNumber).Data.ToString();

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(number) - int.Parse(number2);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Multiply)
                    {
                        if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] != ERPNType.A_Number.ToString() || variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                if (variables[var1][0] == ERPNType.A_Number.ToString())
                                {
                                    RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                    rPNNumber.Data = int.Parse(variables[var1][1]) * int.Parse(variables[var2][1]);
                                    stack.Push(rPNNumber);
                                }
                                else
                                {
                                    throw new Exception("Неверный тип аргументов");
                                }
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number = (stack.Pop() as RPNNumber).Data.ToString();

                                if (!variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var2][1]) * int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var][1]) * int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number2 = (stack.Pop() as RPNNumber).Data.ToString();

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(number) * int.Parse(number2);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Divide)
                    {
                        if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] != ERPNType.A_Number.ToString() || variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                if (variables[var1][0] == ERPNType.A_Number.ToString())
                                {
                                    RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                    rPNNumber.Data = int.Parse(variables[var1][1]) / int.Parse(variables[var2][1]);
                                    stack.Push(rPNNumber);
                                }
                                else
                                {
                                    throw new Exception("Неверный тип аргументов");
                                }
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number = (stack.Pop() as RPNNumber).Data.ToString();

                                if (!variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(number) / int.Parse(variables[var2][1]);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();

                            if (int.Parse(number) == 0)
                            {
                                throw new Exception("Деление на ноль");
                            }

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var][1]) / int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number2 = (stack.Pop() as RPNNumber).Data.ToString();

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(number2) / int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Modulus)
                    {
                        if (stack.Peek() is RPNIdentifier)
                        {
                            string var2 = (stack.Pop() as RPNIdentifier).Name;

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var1 = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var1) || !variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var1][0] != ERPNType.A_Number.ToString() || variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                if (variables[var1][0] == ERPNType.A_Number.ToString())
                                {
                                    RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                    rPNNumber.Data = int.Parse(variables[var1][1]) % int.Parse(variables[var2][1]);
                                    stack.Push(rPNNumber);
                                }
                                else
                                {
                                    throw new Exception("Неверный тип аргументов");
                                }
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number = (stack.Pop() as RPNNumber).Data.ToString();

                                if (!variables.ContainsKey(var2))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var2][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(number) % int.Parse(variables[var2][1]);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else if (stack.Peek() is RPNNumber)
                        {
                            string number = (stack.Pop() as RPNNumber).Data.ToString();

                            if (int.Parse(number) == 0)
                            {
                                throw new Exception("Деление на ноль");
                            }

                            if (stack.Peek() is RPNIdentifier)
                            {
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (variables[var][0] != ERPNType.A_Number.ToString())
                                {
                                    throw new Exception("Несоответствие типов");
                                }

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(variables[var][1]) % int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else if (stack.Peek() is RPNNumber)
                            {
                                string number2 = (stack.Pop() as RPNNumber).Data.ToString();

                                RPNNumber rPNNumber = new RPNNumber(ERPNType.A_Number);
                                rPNNumber.Data = int.Parse(number2) % int.Parse(number);
                                stack.Push(rPNNumber);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                        else { throw new Exception("Неверный тип аргументов"); }
                    }
                    else if (symbol.RPNType == ERPNType.F_Index)
                    {
                        if (stack.Peek() is RPNNumber)
                        {
                            RPNNumber rPNNumber = stack.Peek() as RPNNumber;
                            string index = (stack.Pop() as RPNNumber).Data.ToString();
                            if (stack.Peek() is RPNIdentifier)
                            {
                                RPNIdentifier rPNIdentifier = stack.Peek() as RPNIdentifier;
                                string var = (stack.Pop() as RPNIdentifier).Name;

                                if (!variables.ContainsKey(var))
                                {
                                    throw new Exception("Обращение к несуществующей переменной!");
                                }

                                if (int.Parse(index) >= int.Parse(variables[var][1]))
                                {
                                    throw new Exception("Индекс выходит за границы массива");
                                }

                                stack.Push(rPNIdentifier);
                                stack.Push(rPNNumber);
                                RPNSymbol rPNSymbol = new RPNSymbol(ERPNType.F_Index);
                                stack.Push(rPNSymbol);
                            }
                            else
                            {
                                throw new Exception("Неверный тип аргументов");
                            }
                        }
                    }
                    else if (symbol.RPNType == ERPNType.F_Output)
                    {
                        RPNIdentifier rPNIdentifier = stack.Pop() as RPNIdentifier;

                        foreach (var item in variables[rPNIdentifier.Name])
                        {
                            Console.Write(item + " ");
                        }
                        Console.WriteLine();
                    }
                    else if (symbol.RPNType == ERPNType.F_Input)
                    {
                        if (stack.Peek() is RPNIdentifier)
                        {
                            RPNIdentifier rPNIdentifier = stack.Pop() as RPNIdentifier;

                            string input = Console.ReadLine();

                            if (variables.ContainsKey(rPNIdentifier.Name))
                            {
                                if (variables[rPNIdentifier.Name][0] == ERPNType.A_Number.ToString())
                                {
                                    variables[rPNIdentifier.Name][1] = input;
                                }
                                else if (variables[rPNIdentifier.Name][0] == ERPNType.A_TextLine.ToString())
                                {
                                    variables[rPNIdentifier.Name][1] = input;
                                }
                                else if (variables[rPNIdentifier.Name][0] == ERPNType.A_Boolean.ToString())
                                {
                                    variables[rPNIdentifier.Name][1] = input;
                                }
                                else
                                {
                                    throw new Exception("Неверный тип переменной");
                                }
                            }
                            else
                            {
                                throw new Exception("Обращение к несуществующей переменной");
                            }
                        }
                        else
                        {
                            throw new Exception("Неверный тип аргументов");
                        }
                    }
                }
                currentLineNumber++;
            }
        }
    }
}
