namespace Компилятор
{
    // Главный класс программы, отвечающий за запуск и координацию всех этапов компиляции.
    public static class Programm
    {
        // Точка входа в программу.
        public static void Main()
        {
            // Файл должен находиться в папке: Компилятор\bin\Debug\net8.0\
            string testFileName = "test_formulas.txt";
            //string testFileName = "test_arrays.txt";
            //string testFileName = "test_errors.txt"; 
            //string testFileName = "err_undeclared_var.txt"; 
            //string testFileName = "err_array_as_condition.txt"; 
            //string testFileName = "err_string_as_array_size.txt";
            //string testFileName = "err_invalid_operator.txt"; 
            //string testFileName = "err_unbalanced_parentheses.txt"; 
            //string testFileName = "err_missing_semicolon.txt"; 
            //string testFileName = "err_index_out_of_bounds.txt"; 
            //string testFileName = "err_type_mismatch_op.txt";
            //string testFileName = "err_type_mismatch_assign.txt"; 

            string filePath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, testFileName);

            // Чтение исходного кода из файла
            var code = FileReader.Read(filePath);


            List<Terminal> terminals;
            // Лексический анализ кода
            if (LexicalAnalyzer.IsLexicalCorrect(code))
            {
                terminals = LexicalAnalyzer.GetTerminals();
            }
            else
            {
                // В случае лексической ошибки, выбрасывается исключение
                throw new Exception("Лексическая ошибка в коде.");
            }

            List<RPNSymbol> rpn;
            // Синтаксический анализ списка терминалов
            if (SyntacticalAnalyzer.IsSyntacticalCorrect(terminals))
            {
                // Если синтаксис корректен, транслируем терминалы в обратную польскую нотацию (RPN)
                rpn = RPNTranslator.ConvertToRPN(terminals);
            }
            else
            {
                // В случае синтаксической ошибки, выбрасывается исключение
                throw new Exception("Синтаксическая ошибка в коде.");
            }
            int i = 0;
            // Вывод полученной RPN на консоль (для отладки)
            foreach (var rpnsymvol in rpn)
            {
                Console.Write($"{i++,3} {rpnsymvol.RPNType}");
                if (rpnsymvol is RPNMark)
                {
                    var r = rpnsymvol as RPNMark;
                    Console.Write(" " + r.Position.ToString());
                }
                Console.WriteLine();
            }
            Console.WriteLine("\n\n\n\n");
            // Интерпретация и выполнение инструкций RPN
            RPNInterpreter.ExecuteInstructions(rpn);

        }
    }
}