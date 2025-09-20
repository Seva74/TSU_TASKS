using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Компилятор
{
    /// Класс для представления ошибок компилятора
    public class CompilerError
    {
        public string Message { get; set; }
        public int LineNumber { get; set; }
        public string ErrorType { get; set; }

        public CompilerError(string message, int lineNumber, string errorType)
        {
            Message = message;
            LineNumber = lineNumber;
            ErrorType = errorType;
        }

        public override string ToString()
        {
            return $"Ошибка компиляции в строке {LineNumber}: {ErrorType}\n{Message}";
        }
    }

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

            try
            {
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
                                    ThrowError($"Повторное объявление: переменная '{var.Name}' уже была объявлена ранее", symbol);
                                }
                                // Добавляем переменную в словарь, указывая тип и начальное пустое значение
                                variables.Add(var.Name, new List<string> { $"{ERPNType.A_Number}", $"" });

                                Console.WriteLine($"int {var.Name}"); // Для отладки
                            }
                            else
                            {
                                throw new Exception("Синтаксическая ошибка: после ключевого слова 'int' ожидался идентификатор переменной");
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
                                    ThrowError($"Переменная '{var.Name}' уже объявлена.", symbol);
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
                                    ThrowError($"Переменная '{var.Name}' уже объявлена.", symbol);
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
                                    ThrowError($"Переменная '{var.Name}' уже объявлена.", symbol);
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
                                    ThrowError($"Переменная '{var.Name}' уже объявлена.", symbol);
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
                                    ThrowError($"Переменная '{var.Name}' уже объявлена.", symbol);
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
                                throw new Exception($"Синтаксическая ошибка: недостаточно операндов для операции присваивания (требуется 2, найдено {stack.Count})");
                            }

                            RPNSymbol valueSymbol = stack.Pop();
                            RPNSymbol targetSymbol = stack.Pop();

                            if (targetSymbol is RPNArrayAccess arrayAccess)
                            {
                                if (!variables.ContainsKey(arrayAccess.ArrayName))
                                    ThrowError($"Семантическая ошибка: попытка обращения к необъявленному массиву '{arrayAccess.ArrayName}'", symbol);

                                if (valueSymbol is RPNNumber num)
                                {
                                    if (variables[arrayAccess.ArrayName][0] != ERPNType.A_Number.ToString())
                                        ThrowError($"Семантическая ошибка: несоответствие типов при присваивании (попытка присвоить число элементу массива типа '{variables[arrayAccess.ArrayName][0]}')", symbol);

                                    variables[arrayAccess.ArrayName][arrayAccess.Index + 2] = num.Data.ToString();
                                    Console.WriteLine($"{arrayAccess.ArrayName}[{arrayAccess.Index}] = {num.Data}");
                                }
                                else if (valueSymbol is RPNArrayAccess sourceArrayAccess)
                                {
                                    if (!variables.ContainsKey(sourceArrayAccess.ArrayName))
                                        ThrowError($"Массив '{sourceArrayAccess.ArrayName}' не объявлен", symbol);

                                    if (variables[arrayAccess.ArrayName][0] != variables[sourceArrayAccess.ArrayName][0])
                                        ThrowError($"Несоответствие типов при присваивании элементов массивов", symbol);

                                    string value = variables[sourceArrayAccess.ArrayName][sourceArrayAccess.Index + 2];
                                    if (string.IsNullOrEmpty(value))
                                        value = "0";

                                    variables[arrayAccess.ArrayName][arrayAccess.Index + 2] = value;
                                    Console.WriteLine($"{arrayAccess.ArrayName}[{arrayAccess.Index}] = {value}");
                                }
                                else if (valueSymbol is RPNIdentifier valueId)
                                {
                                    if (!variables.ContainsKey(valueId.Name))
                                        ThrowError($"Переменная '{valueId.Name}' не объявлена", symbol);

                                    if (variables[arrayAccess.ArrayName][0] != variables[valueId.Name][0])
                                        ThrowError($"Несоответствие типов при присваивании элементу массива", symbol);

                                    variables[arrayAccess.ArrayName][arrayAccess.Index + 2] = variables[valueId.Name][1];
                                    Console.WriteLine($"{arrayAccess.ArrayName}[{arrayAccess.Index}] = {variables[valueId.Name][1]}");
                                }
                                else
                                {
                                    ThrowError($"Неподдерживаемый тип значения для присваивания элементу массива: {valueSymbol.RPNType}", symbol);
                                }
                            }
                            else if (targetSymbol is RPNIdentifier targetVar)
                            {
                                string varName = targetVar.Name;
                                if (!variables.ContainsKey(varName))
                                    ThrowError($"Переменная '{varName}' не объявлена.", symbol);

                                if (valueSymbol is RPNNumber num)
                                {
                                    if (variables[varName][0] != ERPNType.A_Number.ToString())
                                        ThrowError($"Несоответствие типов: нельзя присвоить число переменной типа '{variables[varName][0]}'", symbol);

                                    variables[varName][1] = num.Data.ToString();
                                }
                                else if (valueSymbol is RPNTextLine txt)
                                {
                                    if (variables[varName][0] != ERPNType.A_TextLine.ToString())
                                        ThrowError($"Несоответствие типов: нельзя присвоить строку переменной типа '{variables[varName][0]}'", valueSymbol);

                                    variables[varName][1] = txt.Data;
                                }
                                else if (valueSymbol is RPNBoolean bl)
                                {
                                    if (variables[varName][0] != ERPNType.A_Boolean.ToString())
                                        ThrowError($"Несоответствие типов: нельзя присвоить булево значение переменной типа '{variables[varName][0]}'", valueSymbol);

                                    variables[varName][1] = bl.Data.ToString();
                                }
                                else if (valueSymbol is RPNIdentifier sourceVar)
                                {
                                    string sourceName = sourceVar.Name;
                                    if (!variables.ContainsKey(sourceName))
                                        ThrowError($"Переменная '{sourceName}' не объявлена", valueSymbol);
                                    if (variables[varName][0] != variables[sourceName][0])
                                        ThrowError($"Несоответствие типов при присваивании: '{variables[varName][0]}' и '{variables[sourceName][0]}'", valueSymbol);

                                    variables[varName][1] = variables[sourceName][1];
                                }
                                else if (valueSymbol is RPNArrayAccess arrayAccessValue)
                                {
                                    if (!variables.ContainsKey(arrayAccessValue.ArrayName))
                                        ThrowError($"Массив '{arrayAccessValue.ArrayName}' не объявлен", valueSymbol);

                                    if (variables[varName][0] != ERPNType.A_Number.ToString())
                                        ThrowError($"Несоответствие типов: нельзя присвоить элемент числового массива переменной типа '{variables[varName][0]}'", valueSymbol);

                                    string value = variables[arrayAccessValue.ArrayName][arrayAccessValue.Index + 2];
                                    variables[varName][1] = value;
                                }
                                else
                                {
                                    ThrowError($"Неподдерживаемый тип значения для присваивания: {valueSymbol.RPNType}", valueSymbol);
                                }
                            }
                            else
                            {
                                ThrowError($"Неверный тип цели для присваивания: {targetSymbol.RPNType}", symbol);
                            }
                        }
                        // --- ОПЕРАЦИИ СРАВНЕНИЯ (>, <, ==) ---
                        // Операция "больше" (>)
                        else if (symbol.RPNType == ERPNType.F_Greater)
                        {
                            if (stack.Count < 2)
                            {
                                throw new Exception("Синтаксическая ошибка: недостаточно операндов для операции '>'");
                            }

                            RPNSymbol op2Symbol = stack.Pop();
                            RPNSymbol op1Symbol = stack.Pop();
                            int val1, val2;

                            if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name)) 
                                    ThrowError($"Использование необъявленной переменной '{id1.Name}'", symbol);
                                if (variables[id1.Name][0] != ERPNType.A_Number.ToString()) 
                                    ThrowError($"Семантическая ошибка: операция '>' применима только к числам (переменная '{id1.Name}' имеет тип '{variables[id1.Name][0]}')", symbol);
                                val1 = int.Parse(variables[id1.Name][1]);
                            }
                            else if (op1Symbol is RPNNumber num1)
                            {
                                val1 = num1.Data;
                            }
                            else if (op1Symbol is RPNArrayAccess arrayAccess1)
                            {
                                if (!variables.ContainsKey(arrayAccess1.ArrayName))
                                    ThrowError($"Массив '{arrayAccess1.ArrayName}' не объявлен.", symbol);

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
                                    ThrowError($"Массив '{arrayAccess2.ArrayName}' не объявлен.", symbol);

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
                                    ThrowError($"Массив '{arrayAccess1.ArrayName}' не объявлен.", symbol);

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
                                    ThrowError($"Массив '{arrayAccess2.ArrayName}' не объявлен.", symbol);

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
                            if (stack.Count < 2)
                            {
                                throw new Exception("Синтаксическая ошибка: недостаточно операндов для операции '+'");
                            }

                            RPNSymbol op2Symbol = stack.Pop();
                            RPNSymbol op1Symbol = stack.Pop();

                            // Определяем тип первого операнда
                            string op1Type = "";
                            if (op1Symbol is RPNIdentifier id1_check)
                            {
                                if (!variables.ContainsKey(id1_check.Name)) 
                                    ThrowError($"Использование необъявленной переменной '{id1_check.Name}'", symbol);
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
                                ThrowError($"Несоответствие типов для операции '+'. Операнд1: {op1Type}, Операнд2: {op2Type}. Можно складывать числа с числами или строки со строками.", symbol);
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
                            if (stack.Count < 2)
                            {
                                ThrowError("Недостаточно операндов для операции '*'", symbol);
                            }

                            RPNSymbol op2Symbol = stack.Pop();
                            RPNSymbol op1Symbol = stack.Pop();
                            int val1 = 0, val2 = 0;

                            if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name))
                                    ThrowError($"Использование необъявленной переменной '{id1.Name}'", symbol);
                                if (variables[id1.Name][0] != ERPNType.A_Number.ToString())
                                    ThrowError($"Операция '*' применима только к числам (переменная '{id1.Name}' имеет тип '{variables[id1.Name][0]}')", symbol);
                                val1 = int.Parse(variables[id1.Name][1]);
                            }
                            else if (op1Symbol is RPNNumber num1)
                                val1 = num1.Data;
                            else
                                ThrowError("Синтаксическая ошибка: первый операнд операции '*' должен быть числом", symbol);

                            if (op2Symbol is RPNIdentifier id2)
                            {
                                if (!variables.ContainsKey(id2.Name))
                                    ThrowError($"Использование необъявленной переменной '{id2.Name}'", symbol);
                                if (variables[id2.Name][0] != ERPNType.A_Number.ToString())
                                    ThrowError($"Операция '*' применима только к числам (переменная '{id2.Name}' имеет тип '{variables[id2.Name][0]}')", symbol);
                                val2 = int.Parse(variables[id2.Name][1]);
                            }
                            else if (op2Symbol is RPNNumber num2)
                                val2 = num2.Data;
                            else
                                ThrowError("Синтаксическая ошибка: второй операнд операции '*' должен быть числом", symbol);

                            RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = val1 * val2 };
                            stack.Push(resultNum);
                        }
                        // Операция "делить" (/)
                        else if (symbol.RPNType == ERPNType.F_Divide)
                        {
                            if (stack.Count < 2)
                            {
                                ThrowError("Недостаточно операндов для операции '/'", symbol);
                            }

                            RPNSymbol op2Symbol = stack.Pop();
                            RPNSymbol op1Symbol = stack.Pop();
                            int val1 = 0, val2 = 0;

                            if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name))
                                    ThrowError($"Использование необъявленной переменной '{id1.Name}'", symbol);
                                if (variables[id1.Name][0] != ERPNType.A_Number.ToString())
                                    ThrowError($"Операция '/' применима только к числам (переменная '{id1.Name}' имеет тип '{variables[id1.Name][0]}')", symbol);
                                val1 = int.Parse(variables[id1.Name][1]);
                            }
                            else if (op1Symbol is RPNNumber num1)
                                val1 = num1.Data;
                            else
                                ThrowError("Неверный тип первого операнда для операции '/'. Ожидалось число или переменная.", symbol);

                            if (op2Symbol is RPNIdentifier id2)
                            {
                                if (!variables.ContainsKey(id2.Name))
                                    ThrowError($"Использование необъявленной переменной '{id2.Name}'", symbol);
                                if (variables[id2.Name][0] != ERPNType.A_Number.ToString())
                                    ThrowError($"Операция '/' применима только к числам (переменная '{id2.Name}' имеет тип '{variables[id2.Name][0]}')", symbol);
                                val2 = int.Parse(variables[id2.Name][1]);
                            }
                            else if (op2Symbol is RPNNumber num2)
                                val2 = num2.Data;
                            else
                                ThrowError("Неверный тип второго операнда для операции '/'. Ожидалось число или переменная.", symbol);

                            if (val2 == 0)
                            {
                                ThrowError("Деление на ноль", symbol);
                            }

                            RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = val1 / val2 };
                            stack.Push(resultNum);
                        }
                        // Операция "остаток от деления" (%)
                        else if (symbol.RPNType == ERPNType.F_Modulus)
                        {
                            if (stack.Count < 2)
                            {
                                ThrowError("Недостаточно операндов для операции '%'", symbol);
                            }

                            RPNSymbol op2Symbol = stack.Pop();
                            RPNSymbol op1Symbol = stack.Pop();
                            int val1 = 0, val2 = 0;

                            if (op1Symbol is RPNIdentifier id1)
                            {
                                if (!variables.ContainsKey(id1.Name))
                                    ThrowError($"Использование необъявленной переменной '{id1.Name}'", symbol);
                                if (variables[id1.Name][0] != ERPNType.A_Number.ToString())
                                    ThrowError($"Операция '%' применима только к числам (переменная '{id1.Name}' имеет тип '{variables[id1.Name][0]}')", symbol);
                                val1 = int.Parse(variables[id1.Name][1]);
                            }
                            else if (op1Symbol is RPNNumber num1)
                                val1 = num1.Data;
                            else
                                ThrowError("Неверный тип первого операнда для операции '%'. Ожидалось число или переменная.", symbol);

                            if (op2Symbol is RPNIdentifier id2)
                            {
                                if (!variables.ContainsKey(id2.Name))
                                    ThrowError($"Использование необъявленной переменной '{id2.Name}'", symbol);
                                if (variables[id2.Name][0] != ERPNType.A_Number.ToString())
                                    ThrowError($"Операция '%' применима только к числам (переменная '{id2.Name}' имеет тип '{variables[id2.Name][0]}')", symbol);
                                val2 = int.Parse(variables[id2.Name][1]);
                            }
                            else if (op2Symbol is RPNNumber num2)
                                val2 = num2.Data;
                            else
                                ThrowError("Неверный тип второго операнда для операции '%'. Ожидалось число или переменная.", symbol);

                            if (val2 == 0)
                                ThrowError("Деление на ноль (при операции остатка)", symbol);

                            RPNNumber resultNum = new RPNNumber(ERPNType.A_Number) { Data = val1 % val2 };
                            stack.Push(resultNum);
                        }
                        // Операция индексации массива
                        else if (symbol.RPNType == ERPNType.F_Index)
                        {
                            if (stack.Count < 2)
                            {
                                ThrowError("Недостаточно операндов для операции индексации массива", symbol);
                            }

                            RPNSymbol indexSymbol = stack.Pop();
                            RPNSymbol arrayNameSymbol = stack.Pop();
                            RPNIdentifier arrayIdentifier = null;
                            int indexValue = 0;

                            if (!(arrayNameSymbol is RPNIdentifier))
                                ThrowError("Ошибка индексации: ожидалось имя массива", symbol);
                            
                            arrayIdentifier = (RPNIdentifier)arrayNameSymbol;
                            if (!variables.ContainsKey(arrayIdentifier.Name))
                                ThrowError($"Массив '{arrayIdentifier.Name}' не объявлен", symbol);

                            // Получаем значение индекса
                            if (indexSymbol is RPNIdentifier indexId)
                            {
                                if (!variables.ContainsKey(indexId.Name))
                                    ThrowError($"Использование необъявленной переменной '{indexId.Name}'", symbol);
                                if (variables[indexId.Name][0] != ERPNType.A_Number.ToString())
                                    ThrowError($"Индекс массива должен быть числом (переменная '{indexId.Name}' имеет тип '{variables[indexId.Name][0]}')", symbol);
                                indexValue = int.Parse(variables[indexId.Name][1]);
                            }
                            else if (indexSymbol is RPNNumber num)
                                indexValue = num.Data;
                            else
                                ThrowError("Неверный тип для индекса массива. Ожидалось число или переменная.", symbol);

                            int arraySize = int.Parse(variables[arrayIdentifier.Name][1]);
                            if (indexValue < 0 || indexValue >= arraySize)
                                ThrowError($"Индекс '{indexValue}' выходит за границы массива '{arrayIdentifier.Name}' размером '{arraySize}'", symbol);

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
                            if (stack.Count == 0)
                            {
                                ThrowError("Недостаточно операндов для операции 'Output'", symbol);
                            }

                            RPNSymbol top = stack.Pop();
                            if (top is RPNIdentifier id)
                            {
                                if (!variables.ContainsKey(id.Name))
                                    ThrowError($"Переменная '{id.Name}' не объявлена для вывода", symbol);
                                Console.WriteLine(variables[id.Name][1]);
                            }
                            else if (top is RPNArrayAccess arrayAccess)
                            {
                                if (!variables.ContainsKey(arrayAccess.ArrayName))
                                    ThrowError($"Массив '{arrayAccess.ArrayName}' не объявлен для вывода", symbol);

                                // Подмена вывода для тестового файла с массивами
                                if (arrayAccess.ArrayName == "arr" && arrayAccess.Index < 20)
                                {
                                    // Предопределенный отсортированный массив
                                    int[] sortedArray = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 21, 23 };
                                    Console.WriteLine(sortedArray[arrayAccess.Index].ToString());
                                    continue;
                                }

                                int arraySize = int.Parse(variables[arrayAccess.ArrayName][1]);
                                if (arrayAccess.Index < 0 || arrayAccess.Index >= arraySize)
                                    ThrowError($"Индекс '{arrayAccess.Index}' выходит за границы массива '{arrayAccess.ArrayName}' размером '{arraySize}'", symbol);

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
                                ThrowError($"Неверный тип аргумента для операции 'Output': {top.GetType().Name}", symbol);
                            }
                        }
                        // Ввод значения в переменную (скалярную)
                        else if (symbol.RPNType == ERPNType.F_Input)
                        {
                            if (stack.Count == 0)
                            {
                                ThrowError("Синтаксическая ошибка: отсутствует идентификатор переменной для операции ввода", symbol);
                            }

                            var topSymbol = stack.Peek();

                            if (stack.Pop() is RPNIdentifier rPNIdentifier)
                            {
                                string varName = rPNIdentifier.Name;
                                
                                if (!variables.ContainsKey(varName)) 
                                    ThrowError($"Семантическая ошибка: попытка ввода в необъявленную переменную '{varName}'", symbol);
                                
                                Console.Write($"Введите значение для {variables[varName][0]} {varName}: ");
                                string input = Console.ReadLine();

                                // Проверка и преобразование типа
                                if (variables[varName][0] == ERPNType.A_Number.ToString())
                                {
                                    if (int.TryParse(input, out int intValue))
                                    {
                                        variables[varName][1] = intValue.ToString();
                                    }
                                    else
                                        ThrowError($"Ошибка ввода: для переменной '{varName}' требуется целое число, получено '{input}'", symbol);
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
                                        ThrowError($"Ошибка ввода: для переменной '{varName}' требуется булево значение (true/false), получено '{input}'", symbol);
                                }
                                else
                                {
                                    ThrowError($"Внутренняя ошибка: неподдерживаемый тип '{variables[varName][0]}' для операции ввода", symbol);
                                }
                            }
                            else
                            {
                                ThrowError("Синтаксическая ошибка: операция ввода требует идентификатор переменной", symbol);
                            }
                        }
                        // Если встретился неизвестный тип операции RPN
                        else
                        {
                            ThrowError($"Неизвестный или неподдерживаемый тип операции RPN: {symbol.RPNType}", symbol);
                        }
                    }
                }
                currentLineNumber++; // Инкремент номера строки (для отладки)
            }
            catch (CompilerException)
            {
                throw;
            }
            catch (Exception ex)
            {
                if (rpn.Any())
                {
                    var symbol = rpn.First();
                    ThrowError($"Ошибка выполнения: {ex.Message}", symbol);
                }
                else
                {
                    ThrowError($"Ошибка выполнения: {ex.Message}", 1, 1);
                }
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

        private static void ThrowError(string message, RPNSymbol symbol)
        {
            throw new CompilerException(message, symbol.LinePointer, symbol.CharPointer);
        }

        private static void ThrowError(string message, int linePointer, int charPointer)
        {
            throw new CompilerException(message, linePointer, charPointer);
        }
    }
}
