namespace Компилятор
{
    // Главный класс программы, отвечающий за запуск и координацию всех этапов компиляции.
    public static class Programm
    {
        // Точка входа в программу.
        public static void Main()
        {
            try
            {
                // Файл должен находиться в папке: bin\Debug\net8.0\
                //string testFileName = "test_formulas.txt";

                //string testFileName = "test_arrays.txt";

                string testFileName = "err_invalid_operator.txt";
                //string testFileName = "err_undeclared_var.txt"; 
                //string testFileName = "err_type_mismatch_op.txt";
                //string testFileName = "err_type_mismatch_assign.txt"; 
                //string testFileName = "err_index_out_of_bounds.txt";


                //string testFileName = "err_array_as_condition.txt"; 
                //string testFileName = "err_string_as_array_size.txt";
                //string testFileName = "err_unbalanced_parentheses.txt";
                //string testFileName = "err_missing_semicolon.txt"; 

                string filePath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, testFileName);

                // Чтение исходного кода из файла
                var code = FileReader.Read(filePath);
                Console.WriteLine("Исходный код программы:");
                Console.WriteLine(code);
                Console.WriteLine();

                List<Terminal> terminals;
                try
                {
                    // Лексический анализ кода
                    if (!LexicalAnalyzer.IsLexicalCorrect(code))
                    {
                        return;
                    }
                    
                    terminals = LexicalAnalyzer.GetTerminals();

                    // Синтаксический анализ списка терминалов
                    if (!SyntacticalAnalyzer.IsSyntacticalCorrect(terminals))
                    {
                        return;
                    }

                    // Если синтаксис корректен, транслируем терминалы в обратную польскую нотацию (RPN)
                    var rpn = RPNTranslator.ConvertToRPN(terminals);

                    // Интерпретация и выполнение инструкций RPN
                    RPNInterpreter.ExecuteInstructions(rpn);
                }
                catch (CompilerException ex)
                {
                    Console.WriteLine("\nПрограмма завершена с ошибкой:");
                    Console.WriteLine($"Строка {ex.LineNumber}, позиция {ex.CharPosition}: {ex.Message}");
                    return;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("\nПрограмма завершена с неожиданной ошибкой:");
                Console.WriteLine(ex.Message);
            }
        }
    }
}