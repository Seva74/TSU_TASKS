using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Компилятор
{
    /// Класс для представления доступа к элементу массива
    public class RPNArrayAccess : RPNSymbol
    {
        public string ArrayName { get; set; }
        public int Index { get; set; }

        public RPNArrayAccess() : base(ERPNType.F_Index) { }

        public override string ToString()
        {
            return $"ArrayAccess: {ArrayName}[{Index}]";
        }
    }

    /// Класс, отвечающий за интерпретацию и выполнение инструкций в формате обратной польской нотации (RPN).
    public class RPNInterpreter
    {
        /// Выполняет список инструкций RPN.
        /// name="rpn">Список символов RPN для выполнения
        public static void ExecuteInstructions(List<RPNSymbol> rpn)
        {
            // Текущий номер строки (для отладки или будущих расширений, в данной логике не используется активно для управления потоком)
            int currentLineNumber = 0;
            // Номер строки, до которого выполнение команд приостановлено (например, после неудачного условного перехода)
            // В текущей реализации не используется, но может быть полезно для отладки или более сложных сценариев.
            int markedLineNumber = -1; 
            /// Словарь для хранения переменных.
            /// Ключ - имя переменной.
            /// Значение - список строк: 
            ///   - Первый элемент: тип переменной (ERPNType.A_Number, ERPNType.A_TextLine, ERPNType.A_Boolean).
            ///   - Второй элемент: размер массива (для массивов), или пустая строка для скалярных переменных.
            ///   - Последующие элементы (для массивов): значения элементов массива.
            ///   - Для скалярных переменных: второй элемент списка хранит значение переменной.
            var variables = new Dictionary<string, List<string>>();
            /// Стек для выполнения операций RPN.
            /// Сюда помещаются операнды и результаты промежуточных вычислений.
            var stack = new Stack<RPNSymbol>();
            // Счетчик итераций по списку RPN
            int iteration = 0;

            // Основной цикл интерпретатора: проходит по всем символам RPN
            for (; iteration < rpn.Count; iteration++)
            {
                RPNSymbol symbol = rpn[iteration];
                // Условие currentLineNumber >= markedLineNumber в данной реализации всегда истинно,
                // так как markedLineNumber инициализируется -1 и не изменяется. Оставлено для возможной будущей логики.
                if (currentLineNumber >= markedLineNumber)
                {
                    ERPNType type = symbol.RPNType;

                    // Если символ - это операнд (переменная, число, строка, булево) или метка, помещаем его в стек.
                    if (type == ERPNType.A_VariableName ||
                        type == ERPNType.A_Number ||
                        type == ERPNType.A_TextLine ||
                        type == ERPNType.A_Boolean ||
                        type == ERPNType.М_Mark)
                    {
                        stack.Push(symbol);
                    }
                    // --- ОБРАБОТКА ОБЪЯВЛЕНИЯ ПЕРЕМЕННЫХ ---
                    // Объявление переменной типа int
                    else if (symbol.RPNType == ERPNType.F_Int)
                    {
                        // Ожидаем, что на вершине стека имя переменной (идентификатор)
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception($"Переменная '{var.Name}' уже объявлена.");
                            }
                            // Добавляем переменную в словарь, указывая тип и начальное пустое значение
                            variables.Add(var.Name, new List<string> { $"{ERPNType.A_Number}", $"" });

                            Console.WriteLine($"int {var.Name}"); // Для отладки
                        }
                        else
                        {
                            throw new Exception("После типа 'int' ожидался идентификатор переменной.");
                        }
                    }
                    // Объявление переменной типа string
                    else if (symbol.RPNType == ERPNType.F_String)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception($"Переменная '{var.Name}' уже объявлена.");
                            }
                            variables.Add(var.Name, new List<string> { $"{ERPNType.A_TextLine}", $"" });

                            Console.WriteLine($"string {var.Name}");
                        }
                        else
                        {
                            throw new Exception("После типа 'string' ожидался идентификатор переменной.");
                        }
                    }
                    // Объявление переменной типа bool
                    else if (symbol.RPNType == ERPNType.F_Bool)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception($"Переменная '{var.Name}' уже объявлена.");
                            }
                            variables.Add(var.Name, new List<string> { $"{ERPNType.A_Boolean}", $"" });

                            Console.WriteLine($"bool {var.Name}");
                        }
                        else
                        {
                            throw new Exception("После типа 'bool' ожидался идентификатор переменной.");
                        }
                    }
                    // Объявление массива типа int[]
                    else if (symbol.RPNType == ERPNType.F_IntArray)
                    {
                        // Ожидаем на стеке: [имя_массива], [размер_массива]
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception($"Переменная '{var.Name}' уже объявлена.");
                            }

                            if (stack.Peek() is RPNNumber number) // Размер массива
                            {
                                stack.Pop();
                                variables.Add(var.Name, new List<string> { $"{ERPNType.A_Number}", $"{number.Data}" });
                                // Инициализируем элементы массива пустыми строками
                                for (int i = 0; i < number.Data; i++)
                                {
                                    variables[var.Name].Add("");
                                }
                                Console.WriteLine($"int[] {var.Name} = {number.Data}");
                            }
                            else
                            {
                                throw new Exception("После типа 'int[]' ожидалось число элементов массива.");
                            }
                        }
                        else
                        {
                            throw new Exception("После типа 'int[]' ожидался идентификатор массива.");
                        }
                    }
                    // Объявление массива типа string[]
                    else if (symbol.RPNType == ERPNType.F_StringArray)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception($"Переменная '{var.Name}' уже объявлена.");
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
                                throw new Exception("После типа 'string[]' ожидалось число элементов массива.");
                            }
                        }
                        else
                        {
                            throw new Exception("После типа 'string[]' ожидался идентификатор массива.");
                        }
                    }
                    // Объявление массива типа bool[]
                    else if (symbol.RPNType == ERPNType.F_BoolArray)
                    {
                        if (stack.Peek() is RPNIdentifier var)
                        {
                            stack.Pop();
                            if (variables.ContainsKey(var.Name))
                            {
                                throw new Exception($"Переменная '{var.Name}' уже объявлена.");
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
                                throw new Exception("После типа 'bool[]' ожидалось число элементов массива.");
                            }
                        }
                        else
                        {
                            throw new Exception("После типа 'bool[]' ожидался идентификатор массива.");
                        }
                    }
                    // --- ОПЕРАЦИЯ ПРИСВАИВАНИЯ (=) ---
                    else if (symbol.RPNType == ERPNType.F_Assignment)
                    {
                        if (stack.Count < 2)
                        {
                            throw new Exception($"Ошибка в операции присваивания: недостаточно операндов в стеке (требуется минимум 2, найдено {stack.Count})");
                        }

                        RPNSymbol valueSymbol = stack.Pop();
                        RPNSymbol targetSymbol = stack.Pop();

                        if (targetSymbol is RPNArrayAccess arrayAccess)
                        {
                            if (!variables.ContainsKey(arrayAccess.ArrayName))
                                throw new Exception($"Массив '{arrayAccess.ArrayName}' не объявлен");

                            if (valueSymbol is RPNNumber num)
                            {
                                if (variables[arrayAccess.ArrayName][0] != ERPNType.A_Number.ToString())
                                    throw new Exception($"Несоответствие типов: нельзя присвоить число элементу массива типа '{variables[arrayAccess.ArrayName][0]}'");

                                variables[arrayAccess.ArrayName][arrayAccess.Index + 2] = num.Data.ToString();
                                Console.WriteLine($"{arrayAccess.ArrayName}[{arrayAccess.Index}] = {num.Data}");
                            }
                            else if (valueSymbol is RPNArrayAccess sourceArrayAccess)
                            {
                                if (!variables.ContainsKey(sourceArrayAccess.ArrayName))
                                    throw new Exception($"Массив '{sourceArrayAccess.ArrayName}' не объявлен");

                                if (variables[arrayAccess.ArrayName][0] != variables[sourceArrayAccess.ArrayName][0])
                                    throw new Exception($"Несоответствие типов при присваивании элементов массивов");

                                string value = variables[sourceArrayAccess.ArrayName][sourceArrayAccess.Index + 2];
                                if (string.IsNullOrEmpty(value))
                                    value = "0";

                                variables[arrayAccess.ArrayName][arrayAccess.Index + 2] = value;
                                Console.WriteLine($"{arrayAccess.ArrayName}[{arrayAccess.Index}] = {value}");
                            }
                            else if (valueSymbol is RPNIdentifier valueId)
                            {
                                if (!variables.ContainsKey(valueId.Name))
                                    throw new Exception($"Переменная '{valueId.Name}' не объявлена");

                                if (variables[arrayAccess.ArrayName][0] != variables[valueId.Name][0])
                                    throw new Exception($"Несоответствие типов при присваивании элементу массива");

                                variables[arrayAccess.ArrayName][arrayAccess.Index + 2] = variables[valueId.Name][1];
                                Console.WriteLine($"{arrayAccess.ArrayName}[{arrayAccess.Index}] = {variables[valueId.Name][1]}");
                            }
                            else
                            {
                                throw new Exception($"Неподдерживаемый тип значения для присваивания элементу массива: {valueSymbol.RPNType}");
                            }
                        }
                        else if (targetSymbol is RPNIdentifier targetVar)
                        {
                            string varName = targetVar.Name;
                            if (!variables.ContainsKey(varName))
                                throw new Exception($"Переменная '{varName}' не объявлена.");

                            if (valueSymbol is RPNNumber num)
                            {
                                if (variables[varName][0] != ERPNType.A_Number.ToString())
                                    throw new Exception($"Несоответствие типов: нельзя присвоить число переменной типа '{variables[varName][0]}'.");

                                variables[varName][1] = num.Data.ToString();
                                Console.WriteLine($"{varName} = {num.Data}");
                            }
                            else if (valueSymbol is RPNArrayAccess arrayAccessValue)
                            {
                                if (!variables.ContainsKey(arrayAccessValue.ArrayName))
                                    throw new Exception($"Массив '{arrayAccessValue.ArrayName}' не объявлен.");

                                if (variables[varName][0] != ERPNType.A_Number.ToString())
                                    throw new Exception($"Несоответствие типов: нельзя присвоить элемент числового массива переменной типа '{variables[varName][0]}'.");

                                string value = variables[arrayAccessValue.ArrayName][arrayAccessValue.Index + 2];
                                if (string.IsNullOrEmpty(value))
                                    value = "0";

                                variables[varName][1] = value;
                                Console.WriteLine($"{varName} = {value}");
                            }
                            else if (valueSymbol is RPNTextLine txt)
                            {
                                if (variables[varName][0] != ERPNType.A_TextLine.ToString())
                                    throw new Exception($"Несоответствие типов: нельзя присвоить строку переменной типа '{variables[varName][0]}'.");

                                variables[varName][1] = txt.Data;
                                Console.WriteLine($"{varName} = {txt.Data}");
                            }
                            else if (valueSymbol is RPNBoolean bl)
                            {
                                if (variables[varName][0] != ERPNType.A_Boolean.ToString())
                                    throw new Exception($"Несоответствие типов: нельзя присвоить булево значение переменной типа '{variables[varName][0]}'.");

                                variables[varName][1] = bl.Data.ToString();
                                Console.WriteLine($"{varName} = {bl.Data}");
                            }
                            else if (valueSymbol is RPNIdentifier sourceVar)
                            {
                                string sourceName = sourceVar.Name;
                                if (!variables.ContainsKey(sourceName))
                                    throw new Exception($"Переменная '{sourceName}' не объявлена.");
                                if (variables[varName][0] != variables[sourceName][0])
                                    throw new Exception($"Несоответствие типов при присваивании: '{variables[varName][0]}' и '{variables[sourceName][0]}'.");

                                variables[varName][1] = variables[sourceName][1];
                                Console.WriteLine($"{varName} = {variables[sourceName][1]}");
                            }
                            else
                            {
                                throw new Exception($"Неподдерживаемый тип значения для присваивания: {valueSymbol.RPNType}");
                            }
                        }
                        else
                        {
                            throw new Exception($"Неверный тип цели для присваивания: {targetSymbol.RPNType}");
                        }
                    }
                    // --- ОПЕРАЦИИ СРАВНЕНИЯ (>, <, ==) ---
                    // Операция "больше" (>)
                    else if (symbol.RPNType == ERPNType.F_Greater)
                    {
                        RPNSymbol op2Symbol = stack.Pop();
                        RPNSymbol op1Symbol = stack.Pop();
                        int val1, val2;

                        if (op1Symbol is RPNIdentifier id1) 
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '>' применима только к числам. Переменная '{id1.Name}' не число.");
                            val1 = int.Parse(variables[id1.Name][1]);
                        }
                        else if (op1Symbol is RPNNumber num1)
                        {
                            val1 = num1.Data;
                        }
                        else if (op1Symbol is RPNArrayAccess arrayAccess1)
                        {
                            if (!variables.ContainsKey(arrayAccess1.ArrayName))
                                throw new Exception($"Массив '{arrayAccess1.ArrayName}' не объявлен.");

                            string value = variables[arrayAccess1.ArrayName][arrayAccess1.Index + 2];
                            if (string.IsNullOrEmpty(value))
                                value = "0";
                            val1 = int.Parse(value);
                        }
                        else throw new Exception("Неверный тип первого операнда для операции '>'. Ожидалось число, переменная или элемент массива.");

                        if (op2Symbol is RPNIdentifier id2)
                        {
                            if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                            if (variables[id2.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '>' применима только к числам. Переменная '{id2.Name}' не число.");
                            val2 = int.Parse(variables[id2.Name][1]);
                        }
                        else if (op2Symbol is RPNNumber num2)
                        {
                            val2 = num2.Data;
                        }
                        else if (op2Symbol is RPNArrayAccess arrayAccess2)
                        {
                            if (!variables.ContainsKey(arrayAccess2.ArrayName))
                                throw new Exception($"Массив '{arrayAccess2.ArrayName}' не объявлен.");

                            string value = variables[arrayAccess2.ArrayName][arrayAccess2.Index + 2];
                            if (string.IsNullOrEmpty(value))
                                value = "0";
                            val2 = int.Parse(value);
                        }
                        else throw new Exception("Неверный тип второго операнда для операции '>'. Ожидалось число, переменная или элемент массива.");
                        
                        RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean) { Data = val1 > val2 };
                        stack.Push(boolResult);
                    }
                    // Операция "меньше" (<)
                    else if (symbol.RPNType == ERPNType.F_Less)
                    {
                        RPNSymbol op2Symbol = stack.Pop();
                        RPNSymbol op1Symbol = stack.Pop();
                        int val1, val2;

                        if (op1Symbol is RPNIdentifier id1)
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '<' применима только к числам. Переменная '{id1.Name}' не число.");
                            val1 = int.Parse(variables[id1.Name][1]);
                        }
                        else if (op1Symbol is RPNNumber num1)
                        {
                            val1 = num1.Data;
                        }
                        else if (op1Symbol is RPNArrayAccess arrayAccess1)
                        {
                            if (!variables.ContainsKey(arrayAccess1.ArrayName))
                                throw new Exception($"Массив '{arrayAccess1.ArrayName}' не объявлен.");

                            string value = variables[arrayAccess1.ArrayName][arrayAccess1.Index + 2];
                            if (string.IsNullOrEmpty(value))
                                value = "0";
                            val1 = int.Parse(value);
                        }
                        else throw new Exception("Неверный тип первого операнда для операции '<'. Ожидалось число, переменная или элемент массива.");

                        if (op2Symbol is RPNIdentifier id2)
                        {
                            if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                            if (variables[id2.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '<' применима только к числам. Переменная '{id2.Name}' не число.");
                            val2 = int.Parse(variables[id2.Name][1]);
                        }
                        else if (op2Symbol is RPNNumber num2)
                        {
                            val2 = num2.Data;
                        }
                        else if (op2Symbol is RPNArrayAccess arrayAccess2)
                        {
                            if (!variables.ContainsKey(arrayAccess2.ArrayName))
                                throw new Exception($"Массив '{arrayAccess2.ArrayName}' не объявлен.");

                            string value = variables[arrayAccess2.ArrayName][arrayAccess2.Index + 2];
                            if (string.IsNullOrEmpty(value))
                                value = "0";
                            val2 = int.Parse(value);
                        }
                        else throw new Exception("Неверный тип второго операнда для операции '<'. Ожидалось число, переменная или элемент массива.");

                        RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean) { Data = val1 < val2 };
                        stack.Push(boolResult);
                    }
                    // Операция "равно" (==)
                    else if (symbol.RPNType == ERPNType.F_Equal)
                    {
                        RPNSymbol op2Symbol = stack.Pop();
                        RPNSymbol op1Symbol = stack.Pop();
                        bool comparisonResult;

                        // Сравнение чисел
                        if ((op1Symbol is RPNIdentifier || op1Symbol is RPNNumber) && (op2Symbol is RPNIdentifier || op2Symbol is RPNNumber))
                        {
                            int val1, val2;
                            if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                                if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '==' для чисел: переменная '{id1.Name}' не число.");
                                val1 = int.Parse(variables[id1.Name][1]);
                            }
                            else val1 = (op1Symbol as RPNNumber).Data;

                            if (op2Symbol is RPNIdentifier id2)
                            {
                                if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                                if (variables[id2.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '==' для чисел: переменная '{id2.Name}' не число.");
                                val2 = int.Parse(variables[id2.Name][1]);
                            }
                            else val2 = (op2Symbol as RPNNumber).Data;
                            comparisonResult = val1 == val2;
                        }
                        // Сравнение строк
                        else if ((op1Symbol is RPNIdentifier || op1Symbol is RPNTextLine) && (op2Symbol is RPNIdentifier || op2Symbol is RPNTextLine))
                        {
                            string str1, str2;
                            if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                                if (variables[id1.Name][0] != ERPNType.A_TextLine.ToString()) throw new Exception($"Операция '==' для строк: переменная '{id1.Name}' не строка.");
                                str1 = variables[id1.Name][1];
                            }
                            else str1 = (op1Symbol as RPNTextLine).Data;

                            if (op2Symbol is RPNIdentifier id2)
                            {
                                if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                                if (variables[id2.Name][0] != ERPNType.A_TextLine.ToString()) throw new Exception($"Операция '==' для строк: переменная '{id2.Name}' не строка.");
                                str2 = variables[id2.Name][1];
                            }
                            else str2 = (op2Symbol as RPNTextLine).Data;
                            comparisonResult = str1 == str2;
                        }
                        // Сравнение булевых значений
                        else if ((op1Symbol is RPNIdentifier || op1Symbol is RPNBoolean) && (op2Symbol is RPNIdentifier || op2Symbol is RPNBoolean))
                        {
                            bool b1, b2;
                             if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                                if (variables[id1.Name][0] != ERPNType.A_Boolean.ToString()) throw new Exception($"Операция '==' для булевых: переменная '{id1.Name}' не булево.");
                                b1 = bool.Parse(variables[id1.Name][1]);
                            }
                            else b1 = (op1Symbol as RPNBoolean).Data;

                            if (op2Symbol is RPNIdentifier id2)
                            {
                                if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                                if (variables[id2.Name][0] != ERPNType.A_Boolean.ToString()) throw new Exception($"Операция '==' для булевых: переменная '{id2.Name}' не булево.");
                                b2 = bool.Parse(variables[id2.Name][1]);
                            }
                            else b2 = (op2Symbol as RPNBoolean).Data;
                            comparisonResult = b1 == b2;
                        }
                        else
                        {
                            throw new Exception("Несоответствие типов для операции '=='. Можно сравнивать числа с числами, строки со строками или булевы с булевыми.");
                        }
                        stack.Push(new RPNBoolean(ERPNType.A_Boolean) { Data = comparisonResult });
                    }
                    // --- ЛОГИЧЕСКИЕ ОПЕРАЦИИ (&&, ||, !) ---
                    // Логическое "И" (&&)
                    else if (symbol.RPNType == ERPNType.F_And)
                    {
                        RPNSymbol op2Symbol = stack.Pop();
                        RPNSymbol op1Symbol = stack.Pop();
                        bool val1, val2;

                        if (op1Symbol is RPNBoolean bool1) val1 = bool1.Data;
                        else if (op1Symbol is RPNIdentifier id1)
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Boolean.ToString()) throw new Exception($"Операция '&&' применима только к булевым значениям. Переменная '{id1.Name}' не булева.");
                            val1 = bool.Parse(variables[id1.Name][1]);
                        }
                        else throw new Exception("Неверный тип первого операнда для операции '&&'. Ожидалось булево значение.");

                        if (op2Symbol is RPNBoolean bool2) val2 = bool2.Data;
                        else if (op2Symbol is RPNIdentifier id2)
                        {
                            if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                            if (variables[id2.Name][0] != ERPNType.A_Boolean.ToString()) throw new Exception($"Операция '&&' применима только к булевым значениям. Переменная '{id2.Name}' не булева.");
                            val2 = bool.Parse(variables[id2.Name][1]);
                        }
                        else throw new Exception("Неверный тип второго операнда для операции '&&'. Ожидалось булево значение.");

                        RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean) { Data = val1 && val2 };
                        stack.Push(boolResult);
                    }
                    // Логическое "ИЛИ" (||)
                    else if (symbol.RPNType == ERPNType.F_Or)
                    {
                        RPNSymbol op2Symbol = stack.Pop();
                        RPNSymbol op1Symbol = stack.Pop();
                        bool val1, val2;

                        if (op1Symbol is RPNBoolean bool1) val1 = bool1.Data;
                        else if (op1Symbol is RPNIdentifier id1)
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Boolean.ToString()) throw new Exception($"Операция '||' применима только к булевым значениям. Переменная '{id1.Name}' не булева.");
                            val1 = bool.Parse(variables[id1.Name][1]);
                        }
                        else throw new Exception("Неверный тип первого операнда для операции '||'. Ожидалось булево значение.");

                        if (op2Symbol is RPNBoolean bool2) val2 = bool2.Data;
                        else if (op2Symbol is RPNIdentifier id2)
                        {
                            if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                            if (variables[id2.Name][0] != ERPNType.A_Boolean.ToString()) throw new Exception($"Операция '||' применима только к булевым значениям. Переменная '{id2.Name}' не булева.");
                            val2 = bool.Parse(variables[id2.Name][1]);
                        }
                        else throw new Exception("Неверный тип второго операнда для операции '||'. Ожидалось булево значение.");

                        RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean) { Data = val1 || val2 };
                        stack.Push(boolResult);
                    }
                    // Логическое "НЕ" (!)
                    else if (symbol.RPNType == ERPNType.F_Not)
                    {
                        RPNSymbol op1Symbol = stack.Pop();
                        bool val1;

                        if (op1Symbol is RPNBoolean bool1) val1 = bool1.Data;
                        else if (op1Symbol is RPNIdentifier id1)
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Boolean.ToString()) throw new Exception($"Операция '!' применима только к булевым значениям. Переменная '{id1.Name}' не булева.");
                            val1 = bool.Parse(variables[id1.Name][1]);
                        }
                        else throw new Exception("Неверный тип операнда для операции '!'. Ожидалось булево значение.");

                        RPNBoolean boolResult = new RPNBoolean(ERPNType.A_Boolean) { Data = !val1 };
                            stack.Push(boolResult);
                    }
                    // --- УПРАВЛЕНИЕ ПОТОКОМ ВЫПОЛНЕНИЯ (УСЛОВНЫЕ И БЕЗУСЛОВНЫЕ ПЕРЕХОДЫ) ---
                    // Условный переход к метке (используется в if и while)
                    else if (symbol.RPNType == ERPNType.F_ConditionalJumpToMark)
                    {
                        if (stack.Count < 2)
                        {
                            throw new Exception("Ошибка условного перехода: недостаточно операндов в стеке");
                        }

                        RPNMark mark = stack.Pop() as RPNMark;
                        if (mark == null)
                        {
                            throw new Exception("Ошибка условного перехода: на стеке ожидалась метка.");
                        }

                        RPNSymbol conditionSymbol = stack.Pop();
                        bool conditionValue;

                        if (conditionSymbol is RPNBoolean boolResult)
                        {
                            conditionValue = boolResult.Data;
                        }
                        else if (conditionSymbol is RPNIdentifier id)
                        {
                            if (!variables.ContainsKey(id.Name))
                                throw new Exception($"Переменная '{id.Name}' не объявлена.");
                            if (variables[id.Name][0] != ERPNType.A_Boolean.ToString())
                                throw new Exception($"Условие должно быть булевым. Переменная '{id.Name}' не булева.");
                            conditionValue = bool.Parse(variables[id.Name][1]);
                        }
                        else
                        {
                            throw new Exception("Ошибка условного перехода: ожидалось булево условие.");
                        }

                        if (!conditionValue) // Если условие ЛОЖНО, совершаем переход
                        {
                            if (mark.Position == null)
                                throw new Exception($"Ошибка условного перехода: позиция метки '{mark.MarkType}' не установлена.");
                            iteration = mark.Position.Value - 1;
                        }
                    }
                    // Безусловный переход к метке (используется в else и конце while)
                    else if (symbol.RPNType == ERPNType.F_UnconditionalJumpToMark)
                    {
                        RPNMark mark = stack.Pop() as RPNMark;
                        if (mark == null) throw new Exception("Ошибка безусловного перехода: на стеке ожидалась метка.");
                        
                        if (mark.Position == null) throw new Exception($"Ошибка безусловного перехода: позиция метки '{mark.MarkType}' не установлена.");
                        iteration = mark.Position.Value - 1;
                    }
                    // --- АРИФМЕТИЧЕСКИЕ ОПЕРАЦИИ (+, -, *, /, %) ---
                    // Операция "плюс" (+)
                    else if (symbol.RPNType == ERPNType.F_Plus)
                    {
                        RPNSymbol op2Symbol = stack.Pop();
                        RPNSymbol op1Symbol = stack.Pop();

                        // Определяем тип первого операнда
                        string op1Type = "";
                        if (op1Symbol is RPNIdentifier id1_check)
                        {
                            if (!variables.ContainsKey(id1_check.Name)) throw new Exception($"Переменная '{id1_check.Name}' не объявлена.");
                            op1Type = variables[id1_check.Name][0];
                        }
                        else if (op1Symbol is RPNNumber) op1Type = ERPNType.A_Number.ToString();
                        else if (op1Symbol is RPNTextLine) op1Type = ERPNType.A_TextLine.ToString();
                        else if (op1Symbol is RPNBoolean) op1Type = ERPNType.A_Boolean.ToString(); // Добавим обработку булевых, если нужно будет (пока нет операций)
                        else throw new Exception("Неподдерживаемый тип первого операнда для операции '+'.");

                        // Определяем тип второго операнда
                        string op2Type = "";
                        if (op2Symbol is RPNIdentifier id2_check)
                        {
                            if (!variables.ContainsKey(id2_check.Name)) throw new Exception($"Переменная '{id2_check.Name}' не объявлена.");
                            op2Type = variables[id2_check.Name][0];
                        }
                        else if (op2Symbol is RPNNumber) op2Type = ERPNType.A_Number.ToString();
                        else if (op2Symbol is RPNTextLine) op2Type = ERPNType.A_TextLine.ToString();
                        else if (op2Symbol is RPNBoolean) op2Type = ERPNType.A_Boolean.ToString(); // Добавим обработку булевых
                        else throw new Exception("Неподдерживаемый тип второго операнда для операции '+'.");

                        // Случай 1: оба операнда - числа
                        if (op1Type == ERPNType.A_Number.ToString() && op2Type == ERPNType.A_Number.ToString())
                        {
                            int val1, val2;
                            if (op1Symbol is RPNIdentifier id1_num)
                            {
                                val1 = int.Parse(variables[id1_num.Name][1]);
                            }
                            else val1 = (op1Symbol as RPNNumber).Data;

                            if (op2Symbol is RPNIdentifier id2_num)
                            {
                                val2 = int.Parse(variables[id2_num.Name][1]);
                            }
                            else val2 = (op2Symbol as RPNNumber).Data;

                            RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = val1 + val2 };
                            stack.Push(resultNum);
                        }
                        // Случай 2: оба операнда - строки
                        else if (op1Type == ERPNType.A_TextLine.ToString() && op2Type == ERPNType.A_TextLine.ToString())
                        {
                            string str1, str2;
                            if (op1Symbol is RPNIdentifier id1_str)
                            {
                                str1 = variables[id1_str.Name][1];
                            }
                            else str1 = (op1Symbol as RPNTextLine).Data;

                            if (op2Symbol is RPNIdentifier id2_str)
                            {
                                str2 = variables[id2_str.Name][1];
                            }
                            else str2 = (op2Symbol as RPNTextLine).Data;

                            RPNTextLine resultText = new RPNTextLine(ERPNType.A_TextLine) { Data = str1 + str2 };
                            stack.Push(resultText);
                        }
                        else
                        {
                            throw new Exception($"Несоответствие типов для операции '+'. Операнд1: {op1Type}, Операнд2: {op2Type}. Можно складывать числа с числами или строки со строками.");
                        }
                    }
                    // Операция "минус" (-)
                    else if (symbol.RPNType == ERPNType.F_Minus)
                    {
                        RPNSymbol op2Symbol = stack.Pop(); // Вычитаемое
                        
                        // Проверяем, есть ли что-то в стеке
                        if (stack.Count == 0)
                        {
                            // Это точно унарный минус
                            int val;
                            if (op2Symbol is RPNIdentifier id)
                            {
                                if (!variables.ContainsKey(id.Name)) throw new Exception($"Переменная '{id.Name}' не объявлена.");
                                if (variables[id.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Унарный минус применим только к числам. Переменная '{id.Name}' не число.");
                                val = int.Parse(variables[id.Name][1]);
                            }
                            else if (op2Symbol is RPNNumber num) val = num.Data;
                            else throw new Exception("Неверный тип операнда для унарного минуса. Ожидалось число или переменная.");

                            RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = -val };
                            stack.Push(resultNum);
                        }
                        else
                        {
                            // Это бинарный минус
                            RPNSymbol op1Symbol = stack.Pop(); // Уменьшаемое
                            int val1, val2;

                            if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                                if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '-' применима только к числам. Переменная '{id1.Name}' не число.");
                                val1 = int.Parse(variables[id1.Name][1]);
                            }
                            else if (op1Symbol is RPNNumber num1) val1 = num1.Data;
                            else throw new Exception("Неверный тип первого операнда для операции '-'. Ожидалось число или переменная.");

                            if (op2Symbol is RPNIdentifier id2)
                            {
                                if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                                if (variables[id2.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '-' применима только к числам. Переменная '{id2.Name}' не число.");
                                val2 = int.Parse(variables[id2.Name][1]);
                            }
                            else if (op2Symbol is RPNNumber num2) val2 = num2.Data;
                            else throw new Exception("Неверный тип второго операнда для операции '-'. Ожидалось число или переменная.");

                            int result = val1 - val2;
                            RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = result };
                            stack.Push(resultNum);
                        }
                    }
                    // Операция "умножить" (*)
                    else if (symbol.RPNType == ERPNType.F_Multiply)
                    {
                        RPNSymbol op2Symbol = stack.Pop();
                        RPNSymbol op1Symbol = stack.Pop();
                        int val1, val2;

                        if (op1Symbol is RPNIdentifier id1)
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '*' применима только к числам. Переменная '{id1.Name}' не число.");
                            val1 = int.Parse(variables[id1.Name][1]);
                        }
                        else if (op1Symbol is RPNNumber num1) val1 = num1.Data;
                        else throw new Exception("Неверный тип первого операнда для операции '*' Ожидалось число или переменная.");

                        if (op2Symbol is RPNIdentifier id2)
                        {
                            if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                            if (variables[id2.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '*' применима только к числам. Переменная '{id2.Name}' не число.");
                            val2 = int.Parse(variables[id2.Name][1]);
                        }
                        else if (op2Symbol is RPNNumber num2) val2 = num2.Data;
                        else throw new Exception("Неверный тип второго операнда для операции '*' Ожидалось число или переменная.");

                        RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = val1 * val2 };
                        stack.Push(resultNum);
                    }
                    // Операция "разделить" (/)
                    else if (symbol.RPNType == ERPNType.F_Divide)
                    {
                        RPNSymbol op2Symbol = stack.Pop(); // Делитель
                        RPNSymbol op1Symbol = stack.Pop(); // Делимое
                        int val1, val2;

                        if (op1Symbol is RPNIdentifier id1)
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '/' применима только к числам. Переменная '{id1.Name}' не число.");
                            val1 = int.Parse(variables[id1.Name][1]);
                        }
                        else if (op1Symbol is RPNNumber num1) val1 = num1.Data;
                        else throw new Exception("Неверный тип первого операнда для операции '/'. Ожидалось число или переменная.");

                        if (op2Symbol is RPNIdentifier id2)
                        {
                            if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                            if (variables[id2.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '/' применима только к числам. Переменная '{id2.Name}' не число.");
                            val2 = int.Parse(variables[id2.Name][1]);
                        }
                        else if (op2Symbol is RPNNumber num2) val2 = num2.Data;
                        else throw new Exception("Неверный тип второго операнда для операции '/'. Ожидалось число или переменная.");

                        if (val2 == 0) throw new Exception("Деление на ноль.");
                        RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = val1 / val2 };
                        stack.Push(resultNum);
                    }
                    // Операция "остаток от деления" (%)
                    else if (symbol.RPNType == ERPNType.F_Modulus)
                    {
                        RPNSymbol op2Symbol = stack.Pop(); // Делитель
                        RPNSymbol op1Symbol = stack.Pop(); // Делимое
                        int val1, val2;

                        if (op1Symbol is RPNIdentifier id1)
                        {
                            if (!variables.ContainsKey(id1.Name)) throw new Exception($"Переменная '{id1.Name}' не объявлена.");
                            if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '%' применима только к числам. Переменная '{id1.Name}' не число.");
                            val1 = int.Parse(variables[id1.Name][1]);
                        }
                        else if (op1Symbol is RPNNumber num1) val1 = num1.Data;
                        else throw new Exception("Неверный тип первого операнда для операции '%'. Ожидалось число или переменная.");

                        if (op2Symbol is RPNIdentifier id2)
                        {
                            if (!variables.ContainsKey(id2.Name)) throw new Exception($"Переменная '{id2.Name}' не объявлена.");
                            if (variables[id2.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Операция '%' применима только к числам. Переменная '{id2.Name}' не число.");
                            val2 = int.Parse(variables[id2.Name][1]);
                        }
                        else if (op2Symbol is RPNNumber num2) val2 = num2.Data;
                        else throw new Exception("Неверный тип второго операнда для операции '%'. Ожидалось число или переменная.");

                        if (val2 == 0) throw new Exception("Деление на ноль (при операции остатка).");
                        RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = val1 % val2 };
                        stack.Push(resultNum);
                    }
                    // --- ДОСТУП К ЭЛЕМЕНТУ МАССИВА ПО ИНДЕКСУ (F_Index) ---
                    // Эта операция подготавливает стек для последующего присваивания или использования значения элемента.
                    // На стек помещаются: [имя_массива], [индекс], [F_Index].
                    // Фактическое извлечение/запись значения происходит в операциях присваивания или когда имя массива используется в выражении.
                    else if (symbol.RPNType == ERPNType.F_Index)
                    {
                        RPNSymbol indexSymbol = stack.Pop();
                        RPNSymbol arrayNameSymbol = stack.Pop();
                        int indexValue;

                        if (!(arrayNameSymbol is RPNIdentifier arrayIdentifier)) 
                            throw new Exception("Ошибка индексации: ожидалось имя массива.");
                        
                        if (!variables.ContainsKey(arrayIdentifier.Name)) 
                            throw new Exception($"Массив '{arrayIdentifier.Name}' не объявлен.");

                        // Получаем значение индекса
                        if (indexSymbol is RPNIdentifier id)
                        {
                            if (!variables.ContainsKey(id.Name)) throw new Exception($"Переменная индекса '{id.Name}' не объявлена.");
                            if (variables[id.Name][0] != ERPNType.A_Number.ToString()) throw new Exception($"Индекс массива должен быть числом. Переменная '{id.Name}' не число.");
                            indexValue = int.Parse(variables[id.Name][1]);
                        }
                        else if (indexSymbol is RPNNumber num) indexValue = num.Data;
                        else throw new Exception("Неверный тип для индекса массива. Ожидалось число или переменная.");
                        
                        int arraySize = int.Parse(variables[arrayIdentifier.Name][1]);
                        if (indexValue < 0 || indexValue >= arraySize) 
                            throw new Exception($"Индекс '{indexValue}' выходит за границы массива '{arrayIdentifier.Name}' размером '{arraySize}'.");
                        
                        // Создаем специальный объект для индексации массива
                        var arrayAccess = new RPNArrayAccess
                        {
                            ArrayName = arrayIdentifier.Name,
                            Index = indexValue
                        };
                        stack.Push(arrayAccess);
                    }
                    // --- ОПЕРАЦИИ ВВОДА-ВЫВОДА (Output, Input) ---
                    // Вывод значения переменной или элемента массива
                    else if (symbol.RPNType == ERPNType.F_Output)
                    {
                        RPNSymbol top = stack.Pop();

                        if (top is RPNIdentifier id) // Вывод скалярной переменной
                        {
                            if (!variables.ContainsKey(id.Name)) throw new Exception($"Переменная '{id.Name}' не объявлена для вывода.");
                            // Проверяем, это массив или скаляр.
                            // Если количество элементов в списке > 2, это массив ([тип, размер, элемент1, ...])
                            // Иначе это скаляр ([тип, значение])
                            if (variables[id.Name].Count > 2) // Если есть элементы кроме типа и размера/значения, значит массив
                            {
                                Console.Write($"{id.Name}[]: ");
                                for(int k=2; k < variables[id.Name].Count; k++)
                                {
                                    Console.Write(variables[id.Name][k] + (k == variables[id.Name].Count - 1 ? "" : ", "));
                                }
                                Console.WriteLine();
                            }
                            else // Скалярная переменная
                            {
                                Console.WriteLine(variables[id.Name][1]);
                            }
                        }
                        else if (top is RPNArrayAccess arrayAccess) // Вывод элемента массива
                        {
                            if (!variables.ContainsKey(arrayAccess.ArrayName))
                                throw new Exception($"Массив '{arrayAccess.ArrayName}' не объявлен для вывода.");

                            int arraySize = int.Parse(variables[arrayAccess.ArrayName][1]);
                            if (arrayAccess.Index < 0 || arrayAccess.Index >= arraySize)
                                throw new Exception($"Индекс '{arrayAccess.Index}' выходит за границы массива '{arrayAccess.ArrayName}' размером '{arraySize}'.");

                            string value = variables[arrayAccess.ArrayName][arrayAccess.Index + 2];
                            if (string.IsNullOrEmpty(value))
                                value = "0"; // Для неинициализированных элементов возвращаем 0

                            Console.WriteLine(value);
                        }
                        else if (top is RPNNumber num) Console.WriteLine(num.Data.ToString()); // Вывод числового литерала
                        else if (top is RPNTextLine txt) Console.WriteLine(txt.Data); // Вывод строкового литерала
                        else if (top is RPNBoolean bl) Console.WriteLine(bl.Data.ToString()); // Вывод булева литерала
                        else
                        {
                            throw new Exception($"Неверный тип аргумента для операции 'Output': {top.GetType().Name}");
                        }
                    }
                    // Ввод значения в переменную (скалярную)
                    else if (symbol.RPNType == ERPNType.F_Input)
                    {
                        if (stack.Count == 0)
                        {
                            throw new Exception("Ошибка в операции Input: стек пуст, ожидался идентификатор переменной");
                        }

                        var topSymbol = stack.Peek();

                        if (stack.Pop() is RPNIdentifier rPNIdentifier)
                        {
                            string varName = rPNIdentifier.Name;
                            
                            if (!variables.ContainsKey(varName)) 
                                throw new Exception($"Переменная '{varName}' не объявлена для ввода.");
                            
                            Console.Write($"Введите значение для {variables[varName][0]} {varName}: ");
                            string input = Console.ReadLine();

                            // Проверка и преобразование типа (упрощенная)
                            if (variables[varName][0] == ERPNType.A_Number.ToString())
                            {
                                if (int.TryParse(input, out int intValue))
                                {
                                    variables[varName][1] = intValue.ToString();
                                }
                                else
                                    throw new Exception($"Ошибка ввода: ожидалось целое число для переменной '{varName}'.");
                            }
                            else if (variables[varName][0] == ERPNType.A_TextLine.ToString())
                            {
                                variables[varName][1] = input;
                            }
                            else if (variables[varName][0] == ERPNType.A_Boolean.ToString())
                            {
                                if (bool.TryParse(input, out bool boolValue))
                                {
                                    variables[varName][1] = boolValue.ToString();
                                }
                                else
                                    throw new Exception($"Ошибка ввода: ожидалось булево значение (true/false) для переменной '{varName}'.");
                            }
                            else
                            {
                                throw new Exception($"Неподдерживаемый тип переменной '{variables[varName][0]}' для операции Input.");
                            }
                        }
                        else
                        {
                            throw new Exception("Неверный тип аргумента для операции 'Input'. Ожидался идентификатор переменной.");
                        }
                    }
                    // Если встретился неизвестный тип операции RPN
                    else
                    {
                        throw new Exception($"Неизвестный или неподдерживаемый тип операции RPN: {symbol.RPNType}");
                    }
                }
                currentLineNumber++; // Инкремент номера строки (для отладки)
            }
        }

        private static bool IsOperation(ERPNType type)
        {
            return type == ERPNType.F_Plus ||
                   type == ERPNType.F_Minus ||
                   type == ERPNType.F_Multiply ||
                   type == ERPNType.F_Divide ||
                   type == ERPNType.F_Modulus ||
                   type == ERPNType.F_Equal ||
                   type == ERPNType.F_Less ||
                   type == ERPNType.F_Greater ||
                   type == ERPNType.F_LessEqual ||
                   type == ERPNType.F_GreaterEqual ||
                   type == ERPNType.F_And ||
                   type == ERPNType.F_Or;
        }
    }
}
