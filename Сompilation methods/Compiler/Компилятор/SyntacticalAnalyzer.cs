namespace Компилятор
{
    /// Статический класс, отвечающий за синтаксический анализ последовательности терминалов.
    /// Проверяет соответствие кода грамматике языка и строит (неявно) дерево разбора.
    public static class SyntacticalAnalyzer
    {
        /// Строка для логирования процесса синтаксического анализа (для отладки).
        private static string _log = string.Empty;
        
        /// Счетчик для нумерации записей в логе.
        private static int _logCounter = 0;
        /// Строка с текущим уровнем отступов для лога (для лучшей читаемости).
        private static string _tabs = string.Empty;
        /// Счетчик текущего уровня вложенности правил грамматики (для отступов в логе).
        private static int _tabsCounter = 0;
        /// Свойство для управления уровнем отступов в логе синтаксического анализа.
        /// Увеличивает или уменьшает количество символов табуляции в строке `_tabs`.
        public static int Tabs
        {
            get
            {
                return _tabsCounter;
            }
            set
            {
                if (value > _tabsCounter)
                {
                    _tabs += '\t';
                    _tabsCounter = value;
                }
                if (value < _tabsCounter)
                {
                    _tabs = _tabs[1..];
                    _tabsCounter = value;
                }
            }
        }
        /// Свойство для добавления записей в лог синтаксического анализа.
        /// Каждая запись нумеруется и форматируется с текущим уровнем отступов.
        private static string Log
        {
            get
            {
                return _log;
            }
            set
            {
                _log += $"{_logCounter,5}{_tabs}{value}\n";
                _logCounter++;
            }
        }

        /// Запускает синтаксический анализ для предоставленного списка терминалов.
        /// Начинает разбор с правила "Блок инструкций".
        /// Результаты логирования записываются в файл "SAlog.txt".
        /// name="terminals" Список терминалов, полученных от лексического анализатора
        /// <returns>True, если синтаксический анализ прошел успешно, иначе False
        public static bool IsSyntacticalCorrect(List<Terminal> terminals)
        {
            // Начинаем разбор с основного правила грамматики - "Блок инструкций".
            bool result = ParseInstructionBlock(terminals);
            // Записываем лог синтаксического анализа в файл.
            File.WriteAllText("SAlog.txt", Log);
            return result;
        }

        /// Находит индекс парной закрывающей скобки для заданной открывающей скобки.
        /// Поддерживает круглые '()', фигурные '{}' и квадратные '[]' скобки.
        /// <param name="leftParenIndex">Индекс открывающей скобки в списке терминалов.</param>
        /// <param name="terminals">Список терминалов для поиска.</param>
        /// <returns>Индекс парной закрывающей скобки или -1, если пара не найдена.</returns>
        private static int FindPairedClosingBracket(int leftParenIndex, List<Terminal> terminals)
        {
            var openingBracket = terminals[leftParenIndex].TerminalType;
            // Определяем тип ожидаемой закрывающей скобки на основе открывающей.
            var closingBracket = openingBracket switch
            {
                ETerminalType.LeftParen => ETerminalType.RightParen,
                ETerminalType.LeftBrace => ETerminalType.RightBrace,
                ETerminalType.LeftBracket => ETerminalType.RightBracket,
                _ => throw new ArgumentException(),
            };
            int counter = 0; // Счетчик для отслеживания вложенности скобок.
            for (int i = leftParenIndex; i < terminals.Count; i++)
            {
                if (terminals[i].TerminalType == openingBracket)
                {
                    counter++; // Увеличиваем счетчик при встрече открывающей скобки.
                }
                if (terminals[i].TerminalType == closingBracket)
                {
                    counter--; // Уменьшаем счетчик при встрече закрывающей скобки.
                    if (counter == 0) // Если счетчик равен 0, значит найдена парная скобка.
                    {
                        return i;
                    }
                }
            }
            return -1; // Парная скобка не найдена.
        }

        /// 1. Блок инструкций
        private static bool ParseInstructionBlock(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"1. <Блок инструкций> →";
            Tabs++;
            // 1.1 while ( <Логическое или> ) { <Блок инструкций> } <Последующая инструкция>
            Log = $"1.1 while ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> →";
            // если начинается с while
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.While)
            {
                // предполагаемый индекс (
                int leftParenIndex = 1;
                // если по индексу действительно (
                if (terminals.ElementAtOrDefault(leftParenIndex)?.TerminalType == ETerminalType.LeftParen)
                {
                    // находим индекс парной )
                    int rightParenIndex = FindPairedClosingBracket(leftParenIndex, terminals);
                    // если парная ) успешно нашлась
                    if (rightParenIndex != -1)
                    {
                        // предполагаемый индекс {
                        int leftBraceIndex = rightParenIndex + 1;
                        // если по индексу действительно {
                        if (terminals.ElementAtOrDefault(leftBraceIndex)?.TerminalType == ETerminalType.LeftBrace)
                        {
                            // находим индекс парной }
                            int rightBraceIndex = FindPairedClosingBracket(leftBraceIndex, terminals);
                            // если парная } успешно нашлась
                            if (rightBraceIndex != -1)
                            {
                                // выделяем подпоследовательности для парсинга
                                var partForLogicalOR = terminals[(leftParenIndex + 1)..rightParenIndex];
                                var partForInstructionBlock = terminals[(leftBraceIndex + 1)..rightBraceIndex];
                                var partForFollowingInstruction = terminals[(rightBraceIndex + 1)..];
                                // Если подподпоследоватльности терминалов прошли парсинг
                                if (ParseLogicalOR(partForLogicalOR) &&
                                    ParseInstructionBlock(partForInstructionBlock) &&
                                    ParseFollowingInstruction(partForFollowingInstruction))
                                {
                                    Log = $"1.1 while ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> → TRUE";
                                    Tabs--;
                                    Log = $"1. <Блок инструкций> → TRUE";
                                    Tabs--;
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
            Log = $"1.1 while ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> → FALSE";

            // 1.2 if ( <Логическое или> ) { <Блок инструкций> } <Последующая инструкция>
            Log = $"1.2 if ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> →";
            // если начинается с if
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.If)
            {
                // предполагаемый индекс (
                int leftParenIndex = 1;
                // если по индексу действительно (
                if (terminals.ElementAtOrDefault(leftParenIndex)?.TerminalType == ETerminalType.LeftParen)
                {
                    // находим индекс парной )
                    int rightParenIndex = FindPairedClosingBracket(leftParenIndex, terminals);
                    // если парная ) успешно нашлась
                    if (rightParenIndex != -1)
                    {
                        // предполагаемый индекс {
                        int leftBraceIndex = rightParenIndex + 1;
                        // если по индексу действительно {
                        if (terminals.ElementAtOrDefault(leftBraceIndex)?.TerminalType == ETerminalType.LeftBrace)
                        {
                            // находим индекс парной }
                            int rightBraceIndex = FindPairedClosingBracket(leftBraceIndex, terminals);
                            // если парная } успешно нашлась
                            if (rightBraceIndex != -1)
                            {
                                // выделяем подпоследовательности для парсинга
                                var partForLogicalOR = terminals[(leftParenIndex + 1)..rightParenIndex];
                                var partForInstructionBlock = terminals[(leftBraceIndex + 1)..rightBraceIndex];
                                var partForFollowingInstruction = terminals[(rightBraceIndex + 1)..];
                                // Если подподпоследоватльности терминалов прошли парсинг
                                if (ParseLogicalOR(partForLogicalOR) &&
                                    ParseInstructionBlock(partForInstructionBlock) &&
                                    ParseFollowingInstruction(partForFollowingInstruction))
                                {
                                    Log = $"1.2 if ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> → TRUE";
                                    Tabs--;
                                    Log = $"1. <Блок инструкций> → TRUE";
                                    Tabs--;
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
            Log = $"1.2 if ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> → FALSE";

            // 1.3 if ( <Логическое или> ) { <Блок инструкций> } <Последующая инструкция> else { <Блок инструкций> } <Последующая инструкция>
            Log = $"1.3 if ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> else {{ <Блок инструкций> }} <Последующая инструкция> →";
            // если начинается с if
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.If)
            {
                // предполагаемый индекс (
                int leftParenIndex = 1;
                // если по индексу действительно (
                if (terminals.ElementAtOrDefault(leftParenIndex)?.TerminalType == ETerminalType.LeftParen)
                {
                    // находим индекс парной )
                    int rightParenIndex = FindPairedClosingBracket(leftParenIndex, terminals);
                    // если парная ) успешно нашлась
                    if (rightParenIndex != -1)
                    {
                        // предполагаемый индекс {
                        int firstLeftBraceIndex = rightParenIndex + 1;
                        // если по индексу действительно {
                        if (terminals.ElementAtOrDefault(firstLeftBraceIndex)?.TerminalType == ETerminalType.LeftBrace)
                        {
                            // находим индекс парной }
                            int firstRightBraceIndex = FindPairedClosingBracket(firstLeftBraceIndex, terminals);
                            // если парная } успешно нашлась
                            if (firstRightBraceIndex != -1)
                            {
                                // предполагаемый индекс else
                                int elseIndex = firstRightBraceIndex + 1;

                                // если по индексу действительно else
                                if (terminals.ElementAtOrDefault(elseIndex)?.TerminalType == ETerminalType.Else)
                                {
                                    // предполагаемый индекс {
                                    int secondLeftBraceIndex = elseIndex + 1;

                                    // если по индексу действительно {
                                    if (terminals.ElementAtOrDefault(secondLeftBraceIndex)?.TerminalType == ETerminalType.LeftBrace)
                                    {
                                        // находим индекс парной }
                                        int secondRightBraceIndex = FindPairedClosingBracket(secondLeftBraceIndex, terminals);
                                        // если парная } успешно нашлась
                                        if (secondRightBraceIndex != -1)
                                        {
                                            // выделяем подпоследовательности для парсинга
                                            var partForLogicalOR = terminals[(leftParenIndex + 1)..rightParenIndex];
                                            var partForFirstInstructionBlock = terminals[(firstLeftBraceIndex + 1)..firstRightBraceIndex];
                                            var partForSecondInstructionBlock = terminals[(secondLeftBraceIndex + 1)..secondRightBraceIndex];
                                            var partForFollowingInstruction = terminals[(secondRightBraceIndex + 1)..];
                                            // Если подподпоследоватльности терминалов прошли парсинг
                                            if (ParseLogicalOR(partForLogicalOR) &&
                                                ParseInstructionBlock(partForFirstInstructionBlock) &&
                                                ParseInstructionBlock(partForSecondInstructionBlock) &&
                                                ParseFollowingInstruction(partForFollowingInstruction))
                                            {
                                                Log = $"1.3 if ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> else {{ <Блок инструкций> }} <Последующая инструкция> → TRUE";
                                                Tabs--;
                                                Log = $"1. <Блок инструкций> → TRUE";
                                                Tabs--;
                                                return true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Log = $"1.3 if ( <Логическое или> ) {{ <Блок инструкций> }} <Последующая инструкция> else {{ <Блок инструкций> }} <Последующая инструкция> → FALSE";

            // 1.4 Input(<Идентификатор>) ; <Последующая инструкция>
            Log = $"1.4 Input(<Идентификатор>) ; <Последующая инструкция> →";
            // если начинается с Input
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Input)
            {
                // предполагаемый индекс (
                int leftParenIndex = 1;

                // если по индексу действительно (
                if (terminals.ElementAtOrDefault(leftParenIndex)?.TerminalType == ETerminalType.LeftParen)
                {
                    // находим индекс парной )
                    int rightParenIndex = FindPairedClosingBracket(leftParenIndex, terminals);
                    // если парная ) успешно нашлась
                    if (rightParenIndex != -1)
                    {
                        // предполагаемый индекс ;
                        int semicolonIndex = rightParenIndex + 1;

                        // если по индексу действительно ;
                        if (terminals.ElementAtOrDefault(semicolonIndex)?.TerminalType == ETerminalType.Semicolon)
                        {
                            // выделяем подпоследовательности для парсинга
                            var partForIdentifier = terminals[(leftParenIndex + 1)..rightParenIndex];
                            var partForFollowingInstruction = terminals[(semicolonIndex + 1)..];
                            // Если подподпоследоватльности терминалов прошли парсинг
                            if (ParseIdentifier(partForIdentifier) &&
                                ParseFollowingInstruction(partForFollowingInstruction))
                            {
                                Log = $"1.4 Input(<Идентификатор>) ; <Последующая инструкция> → TRUE";
                                Tabs--;
                                Log = $"1. <Блок инструкций> → TRUE";
                                Tabs--;
                                return true;
                            }
                        }
                    }
                }
            }
            Log = $"1.4 Input(<Идентификатор>) ; <Последующая инструкция> → FALSE";

            // 1.5 Output(<Идентификатор>) ; <Последующая инструкция>
            Log = $"1.5 Output(<Идентификатор>) ; <Последующая инструкция> →";
            // если начинается с Output (было Input)
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Output)
            {
                // предполагаемый индекс (
                int leftParenIndex = 1;

                // если по индексу действительно (
                if (terminals.ElementAtOrDefault(leftParenIndex)?.TerminalType == ETerminalType.LeftParen)
                {
                    // находим индекс парной )
                    int rightParenIndex = FindPairedClosingBracket(leftParenIndex, terminals);
                    // если парная ) успешно нашлась
                    if (rightParenIndex != -1)
                    {
                        // предполагаемый индекс ;
                        int semicolonIndex = rightParenIndex + 1;

                        // если по индексу действительно ;
                        if (terminals.ElementAtOrDefault(semicolonIndex)?.TerminalType == ETerminalType.Semicolon)
                        {
                            // выделяем подпоследовательности для парсинга
                            var partForIdentifier = terminals[(leftParenIndex + 1)..rightParenIndex];
                            var partForFollowingInstruction = terminals[(semicolonIndex + 1)..];
                            // Если подподпоследоватльности терминалов прошли парсинг
                            if (ParseIdentifier(partForIdentifier) &&
                                ParseFollowingInstruction(partForFollowingInstruction))
                            {
                                Log = $"1.5 Output(<Идентификатор>) ; <Последующая инструкция> → TRUE";
                                Tabs--;
                                Log = $"1. <Блок инструкций> → TRUE";
                                Tabs--;
                                return true;
                            }
                        }
                    }
                }
            }
            Log = $"1.5 Output(<Идентификатор>) ; <Последующая инструкция> → FALSE";

            // находим индекс первой точки с запятой
            int firstSemicolon = terminals.FindIndex(t => t.TerminalType == ETerminalType.Semicolon);

            // 1.6 <Инициализация переменной> ; <Последующая инструкция>
            Log = $"1.6 <Инициализация переменной> ; <Последующая инструкция> →";
            // если в последовательности есть ;
            if (firstSemicolon != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForVariableInitialization = terminals[..firstSemicolon];
                var partForFollowingInstruction = terminals[(firstSemicolon + 1)..];
                // проверяем подпоследовательности
                if (ParseVariableInitialization(partForVariableInitialization) &&
                    ParseFollowingInstruction(partForFollowingInstruction))
                {
                    Log = $"1.6 <Инициализация переменной> ; <Последующая инструкция> → TRUE";
                    Tabs--;
                    Log = $"1. <Блок инструкций> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"1.6 <Инициализация переменной> ; <Последующая инструкция> → FALSE";

            // 1.7 <Присваивание> ; <Последующая инструкция>
            Log = $"1.7 <Присваивание> ; <Последующая инструкция> →";
            // если в последовательности есть ;
            if (firstSemicolon != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForAssignment = terminals[..firstSemicolon];
                var partForFollowingInstruction = terminals[(firstSemicolon + 1)..];
                // проверяем подпоследовательности
                if (ParseAssignment(partForAssignment) &&
                    ParseFollowingInstruction(partForFollowingInstruction))
                {
                    Log = $"1.7 <Присваивание> ; <Последующая инструкция> → TRUE";
                    Tabs--;
                    Log = $"1. <Блок инструкций> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"1.7 <Присваивание> ; <Последующая инструкция> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"1. <Блок инструкций> → FALSE";
            Tabs--;
            return false;
        }



        /// 2. Последующая инструкция
        // <Последующая инструкция> -> <Блок инструкций>
        //                           | epsilon
        // (По сути, это рекурсивный вызов для обработки следующей полноценной инструкции, 
        //  либо завершение, если инструкций больше нет)
        private static bool ParseFollowingInstruction(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"2. <Последующая инструкция> →";
            Tabs++;
            // если есть хотя бы один терминал, пытаемся разобрать его как <Блок инструкций>
            if (terminals.Count != 0)
            {
                // 2.1 <Блок инструкций>
                Log = $"2.1 <Блок инструкций> →";
                if (ParseInstructionBlock(terminals)) // Рекурсивно вызываем ParseInstructionBlock
                {
                    Log = $"2.1 <Блок инструкций> → TRUE";
                    Tabs--;
                    Log = $"2. <Последующая инструкция> → TRUE";
                    Tabs--;
                    return true;
                }
                Log = $"2.1 <Блок инструкций> → FALSE";
                
                Tabs--;
                Log = $"2. <Последующая инструкция> → FALSE (не удалось разобрать как блок инструкций)";
                Tabs--;
                return false; 
            }
            else // если терминалов нет - это эпсилон-переход (успешное завершение)
            {
                // 2.2 λ (пустая строка)
                Log = $"2.2 λ → TRUE";
                Tabs--;
                Log = $"2. <Последующая инструкция> → TRUE (пусто)";
                Tabs--;
                return true;
            }
        }

        /// 3. Инициализация переменной
        private static bool ParseVariableInitialization(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"3. <Инициализация переменной> →";
            Tabs++;
            //если в последовательности ровно 5 треминалов
            if (terminals.Count == 5)
            {
                // 3.1 int[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ
                Log = $"3.1 int[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ →";
                // если начинается с int
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Int)
                {
                    // если следующий терминал [
                    if (terminals.ElementAtOrDefault(1)?.TerminalType == ETerminalType.LeftBracket)
                    {
                        // если следующий терминал ЧИСЛО
                        if (terminals.ElementAtOrDefault(2)?.TerminalType == ETerminalType.Number)
                        {
                            // если следующий терминал ]
                            if (terminals.ElementAtOrDefault(3)?.TerminalType == ETerminalType.RightBracket)
                            {
                                // если следующий терминал идентификатор
                                if (terminals.ElementAtOrDefault(4)?.TerminalType == ETerminalType.VariableName)
                                {
                                    Log = $"3.1 int[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                                    Tabs--;
                                    Log = $"3. <Инициализация переменной> → TRUE";
                                    Tabs--;
                                    return true;
                                }
                            }
                        }
                    }
                }
                Log = $"3.1 int[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";

                // 3.2 bool[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ
                Log = $"3.2 bool[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ →";
                // если начинается с bool
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Bool)
                {
                    // если следующий терминал [
                    if (terminals.ElementAtOrDefault(1)?.TerminalType == ETerminalType.LeftBracket)
                    {
                        // если следующий терминал ЧИСЛО
                        if (terminals.ElementAtOrDefault(2)?.TerminalType == ETerminalType.Number)
                        {
                            // если следующий терминал ]
                            if (terminals.ElementAtOrDefault(3)?.TerminalType == ETerminalType.RightBracket)
                            {
                                // если следующий терминал идентификатор
                                if (terminals.ElementAtOrDefault(4)?.TerminalType == ETerminalType.VariableName)
                                {
                                    Log = $"3.2 bool[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                                    Tabs--;
                                    Log = $"3. <Инициализация переменной> → TRUE";
                                    Tabs--;
                                    return true;
                                }
                            }
                        }
                    }
                }
                Log = $"3.2 bool[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";

                // 3.3 string[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ
                Log = $"3.3 string[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ →";
                // если начинается с string
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.String)
                {
                    // если следующий терминал [
                    if (terminals.ElementAtOrDefault(1)?.TerminalType == ETerminalType.LeftBracket)
                    {
                        // если следующий терминал ЧИСЛО
                        if (terminals.ElementAtOrDefault(2)?.TerminalType == ETerminalType.Number)
                        {
                            // если следующий терминал ]
                            if (terminals.ElementAtOrDefault(3)?.TerminalType == ETerminalType.RightBracket)
                            {
                                // если следующий терминал идентификатор
                                if (terminals.ElementAtOrDefault(4)?.TerminalType == ETerminalType.VariableName)
                                {
                                    Log = $"3.3 string[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                                    Tabs--;
                                    Log = $"3. <Инициализация переменной> → TRUE";
                                    Tabs--;
                                    return true;
                                }
                            }
                        }
                    }
                }
                Log = $"3.3 string[ЧИСЛО] НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";
            }

            //если в последовательности ровно 2 терминала
            if (terminals.Count == 2)
            {
                // 3.4 int НАЗВАНИЕ ПЕРЕМЕННОЙ
                Log = $"3.4 int НАЗВАНИЕ ПЕРЕМЕННОЙ →";
                // если начинается с int
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Int)
                {
                    // если следующий терминал идентификатор
                    if (terminals.ElementAtOrDefault(1)?.TerminalType == ETerminalType.VariableName)
                    {
                        Log = $"3.4 int НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                        Tabs--;
                        Log = $"3. <Инициализация переменной> → TRUE";
                        Tabs--;
                        return true;
                    }
                }
                Log = $"3.4 int НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";

                // 3.5 bool НАЗВАНИЕ ПЕРЕМЕННОЙ
                Log = $"3.5 bool НАЗВАНИЕ ПЕРЕМЕННОЙ →";
                // если начинается с bool
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Bool)
                {
                    // если следующий терминал идентификатор
                    if (terminals.ElementAtOrDefault(1)?.TerminalType == ETerminalType.VariableName)
                    {
                        Log = $"3.5 bool НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                        Tabs--;
                        Log = $"3. <Инициализация переменной> → TRUE";
                        Tabs--;
                        return true;
                    }
                }
                Log = $"3.5 bool НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";

                // 3.6 string НАЗВАНИЕ ПЕРЕМЕННОЙ
                Log = $"3.6 string НАЗВАНИЕ ПЕРЕМЕННОЙ →";
                // если начинается с string
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.String)
                {
                    // если следующий терминал идентификатор
                    if (terminals.ElementAtOrDefault(1)?.TerminalType == ETerminalType.VariableName)
                    {
                        Log = $"3.6 string НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                        Tabs--;
                        Log = $"3. <Инициализация переменной> → TRUE";
                        Tabs--;
                        return true;
                    }
                }
                Log = $"3.6 string НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";
            }

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"3. <Инициализация переменной> → FALSE";
            Tabs--;
            return false;
        }



        /// 4. Присваивание
        private static bool ParseAssignment(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"4. <Присваивание> →";
            Tabs++;
            // 4.1 <Идентификатор> = <Аргумент присваивания>
            Log = $"4.1 <Идентификатор> = <Аргумент присваивания> →";
            // находим индекс первого =
            int firstAssignment = terminals.FindIndex(t => t.TerminalType == ETerminalType.Assignment);

            // если = нашелся
            if (firstAssignment != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForIdentifier = terminals[..firstAssignment];
                var partForAssignmentArgument = terminals[(firstAssignment + 1)..];
                // проверяем подпоследовательности
                if (ParseIdentifier(partForIdentifier) &&
                    ParseAssignmentArgument(partForAssignmentArgument))
                {
                    Log = $"4.1 <Идентификатор> = <Аргумент присваивания> → TRUE";
                    Tabs--;
                    Log = $"4. <Присваивание> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"4.1 <Идентификатор> = <Аргумент присваивания> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"4. <Присваивание> → FALSE";
            Tabs--;
            return false;
        }

        /// 5. Аргумент присваивания
        private static bool ParseAssignmentArgument(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"5. <Аргумент присваивания> →";
            Tabs++;
            // 5.1 <Логическое ИЛИ>
            Log = $"5.1 <Логическое ИЛИ> →";
            // если посдевовательность удовлетворяет шаблону <Логическое ИЛИ>
            if (ParseLogicalOR(terminals))
            {
                Log = $"5.1 <Логическое ИЛИ> → TRUE";
                Tabs--;
                Log = $"5. <Аргумент присваивания> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"5.1 <Логическое ИЛИ> → FALSE";

            // 5.2 <Конкатенация>
            Log = $"5.2 <Конкатенация> →";
            // если посдевовательность удовлетворяет шаблону <Конкатенация>
            if (ParseConcatenation(terminals))
            {
                Log = $"5.2 <Конкатенация> → TRUE";
                Tabs--;
                Log = $"5. <Аргумент присваивания> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"5.2 <Конкатенация> → FALSE";

            // 5.3 <Сложение и вычитание>
            Log = $"5.3 <Сложение и вычитание> →";
            // если посдевовательность удовлетворяет шаблону <Сложение и вычитание>
            if (ParseAdditionAndSubtraction(terminals))
            {
                Log = $"5.3 <Сложение и вычитание> → TRUE";
                Tabs--;
                Log = $"5. <Аргумент присваивания> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"5.3 <Сложение и вычитание> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"5. <Аргумент присваивания> → FALSE";
            Tabs--;
            return false;
        }

        /// 6. Логическое ИЛИ
        private static bool ParseLogicalOR(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"6. <Логическое ИЛИ> →";
            Tabs++;
            // находим индекс первого ||
            int firstLogicalOR = terminals.FindIndex(t => t.TerminalType == ETerminalType.Or);

            // 6.1 <Логическое И> || <Логическое ИЛИ>
            Log = $"6.1 <Логическое И> || <Логическое ИЛИ> →";
            // если || нашелся
            if (firstLogicalOR != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForLogicalAND = terminals[..firstLogicalOR];
                var partForLogicalOR = terminals[(firstLogicalOR + 1)..];
                // проверяем подпоследовательности
                if (ParseLogicalAND(partForLogicalAND) &&
                    ParseLogicalOR(partForLogicalOR))
                {
                    Log = $"6.1 <Логическое И> || <Логическое ИЛИ> → TRUE";
                    Tabs--;
                    Log = $"6. <Логическое ИЛИ> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"6.1 <Логическое И> || <Логическое ИЛИ> → FALSE";

            // 6.2 <Логическое И>
            Log = $"6.2 <Логическое И> →";
            // если посдевовательность удовлетворяет шаблону <Логическое И>
            if (ParseLogicalAND(terminals))
            {
                Log = $"6.2 <Логическое И> → TRUE";
                Tabs--;
                Log = $"6. <Логическое ИЛИ> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"6.2 <Логическое И> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"6. <Логическое ИЛИ> → FALSE";
            Tabs--;
            return false;
        }

        /// 7. Логическое И
        private static bool ParseLogicalAND(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"7. <Логическое И> →";
            Tabs++;
            // 7.1 <Аргумент логического И> && <Логическое И>
            Log = $"7.1 <Аргумент логического И> && <Логическое И> →";
            // находим индекс первого &&
            int firstLogicalAND = terminals.FindIndex(t => t.TerminalType == ETerminalType.And);
            // если && нашелся
            if (firstLogicalAND != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForLogicalANDArgument = terminals[..firstLogicalAND];
                var partForLogicalOR = terminals[(firstLogicalAND + 1)..];
                // проверяем подпоследовательности
                if (ParseLogicalANDArgument(partForLogicalANDArgument) &&
                    ParseLogicalOR(partForLogicalOR))
                {
                    Log = $"7.1 <Аргумент логического И> && <Логическое И> → TRUE";
                    Tabs--;
                    Log = $"7. <Логическое И> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"7.1 <Аргумент логического И> && <Логическое И> → FALSE";

            // 7.2 <Аргумент логического И>
            Log = $"7.2 <Аргумент логического И> →";
            // если посдевовательность удовлетворяет шаблону <Аргумент логического И>
            if (ParseLogicalANDArgument(terminals))
            {
                Log = $"7.2 <Аргумент логического И> → TRUE";
                Tabs--;
                Log = $"7. <Логическое И> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"7.2 <Аргумент логического И> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"7. <Логическое И> → FALSE";
            Tabs--;
            return false;
        }

        /// 8. Аргумент логического И
        private static bool ParseLogicalANDArgument(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"8. <Аргумент логического И> →";
            Tabs++;
            // 8.1 <Отрицание>
            Log = $"8.1 <Отрицание> →";
            // если посдевовательность удовлетворяет шаблону <Отрицание>
            if (ParseNegation(terminals))
            {
                Log = $"8.1 <Отрицание> → TRUE";
                Tabs--;
                Log = $"8. <Аргумент логического И> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"8.1 <Отрицание> → FALSE";

            // 8.2 <Строковое сравнение>
            Log = $"8.2 <Строковое сравнение> →";
            // если посдевовательность удовлетворяет шаблону <Строковое сравнение>
            if (ParseStringComparison(terminals))
            {
                Log = $"8.2 <Строковое сравнение> → TRUE";
                Tabs--;
                Log = $"8. <Аргумент логического И> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"8.2 <Строковое сравнение> → FALSE";

            // 8.3 <Числовое сравнение>
            Log = $"8.3 <Числовое сравнение> →";
            // если посдевовательность удовлетворяет шаблону <Числовое сравнение>
            if (ParseNumericalComparison(terminals))
            {
                Log = $"8.3 <Числовое сравнение> → TRUE";
                Tabs--;
                Log = $"8. <Аргумент логического И> TRUE";
                Tabs--;
                return true;
            }
            Log = $"8.3 <Числовое сравнение> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"8. <Аргумент логического И> → FALSE";
            Tabs--;
            return false;
        }

        /// 9. Отрицание
        private static bool ParseNegation(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"9. <Отрицание> →";
            Tabs++;
            // 9.1 !<Аргумент отрицания>
            Log = $"9.1 !<Аргумент отрицания> →";
            // если первый терминал !
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Not)
            {
                // выделяем подпоследовательность для парсинга
                var partForNegationArgument = terminals[1..];
                // проверяем подпоследовательности
                if (ParseNegationArgument(partForNegationArgument))
                {
                    Log = $"9.1 !<Аргумент отрицания> → TRUE";
                    Tabs--;
                    Log = $"9. <Отрицание> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"8.3 <Числовое сравнение> → FALSE";

            // 9.2 <Аргумент отрицания>
            Log = $"9.2 <Аргумент отрицания> →";
            // если посдевовательность удовлетворяет шаблону <Аргумент отрицания>
            if (ParseNegationArgument(terminals))
            {
                Log = $"9.2 <Аргумент отрицания> → TRUE";
                Tabs--;
                Log = $"9. <Отрицание> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"9.2 <Аргумент отрицания> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"9. <Отрицание> → FALSE";
            Tabs--;
            return false;
        }

        /// 10. Аргумент отрицания
        private static bool ParseNegationArgument(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"10. <Аргумент отрицания> →";
            Tabs++;
            // 10.1 (<Логическое или>)
            Log = $"10.1 (<Логическое или>) →";
            // если первый терминал (
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.LeftParen)
            {
                // находим индекс парной )
                int rightParenIndex = FindPairedClosingBracket(0, terminals);
                // если парная ) успешно нашлась
                if (rightParenIndex != -1)
                {
                    // если парная ) это последний терминал
                    if (rightParenIndex == terminals.Count - 1)
                    {
                        // выделяем подпоследовательность для парсинга
                        var partForLogicalOr = terminals[1..rightParenIndex];
                        // проверяем подпоследовательности
                        if (ParseLogicalOR(partForLogicalOr))
                        {
                            Log = $"10.1 (<Логическое или>) → TRUE";
                            Tabs--;
                            Log = $"10. <Аргумент отрицания> → TRUE";
                            Tabs--;
                            return true;
                        }
                    }
                }
            }
            Log = $"10.1 (<Логическое или>) → FALSE";

            // 10.2 <Идентификатор>
            Log = $"10.2 <Идентификатор> →";
            // если посдевовательность удовлетворяет шаблону <Идентификатор>
            if (ParseIdentifier(terminals))
            {
                Log = $"10.2 <Идентификатор> → TRUE";
                Tabs--;
                Log = $"10. <Аргумент отрицания> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"10.2 <Идентификатор> → FALSE";

            // 10.3 БУЛЕАН
            Log = $"10.3 БУЛЕАН →";
            // если в последователельности ровно один терминал
            if (terminals.Count == 1)
            {
                // если первый терминал булеан
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Boolean)
                {
                    Log = $"10.3 БУЛЕАН → TRUE";
                    Tabs--;
                    Log = $"10. <Аргумент отрицания> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"10.3 БУЛЕАН → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"10. <Аргумент отрицания> → FALSE";
            Tabs--;
            return false;
        }

        /// 11. Строковое сравнение
        private static bool ParseStringComparison(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"11. <Строковое сравнение> →";
            Tabs++;

            // находим индекс первого ==
            int firstEqual = terminals.FindIndex(t => t.TerminalType == ETerminalType.Equal);

            // 11.1 <Конкатенация> == <Конкатенация>
            Log = $"11.1 <Конкатенация> == <Конкатенация> →";
            // если == нашелся
            if (firstEqual != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForLeftConcatenation = terminals[..firstEqual];
                var partForRightConcatenation = terminals[(firstEqual + 1)..];
                // проверяем подпоследовательности
                if (ParseConcatenation(partForLeftConcatenation) &&
                    ParseConcatenation(partForRightConcatenation))
                {
                    Log = $"11.1 <Конкатенация> == <Конкатенация> → TRUE";
                    Tabs--;
                    Log = $"11. <Строковое сравнение> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"11.1 <Конкатенация> == <Конкатенация> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"11. <Строковое сравнение> → FALSE";
            Tabs--;
            return false;
        }

        /// 12. Конкатенация
        private static bool ParseConcatenation(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"12. <Конкатенация> →";
            Tabs++;
            // 12.1 <Аргумент конкатенации> + <Конкатенация>
            Log = $"12.1 <Аргумент конкатенации> + <Конкатенация> →";
            // находим индекс первого +
            int firstPlus = terminals.FindIndex(t => t.TerminalType == ETerminalType.Plus);
            // если + нашелся
            if (firstPlus != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForConcatenationArgument = terminals[..firstPlus];
                var partForConcatenation = terminals[(firstPlus + 1)..];
                // проверяем подпоследовательности
                if (ParseConcatenationArgument(partForConcatenationArgument) &&
                    ParseConcatenation(partForConcatenation))
                {
                    Log = $"12.1 <Аргумент конкатенации> + <Конкатенация> TRUE";
                    Tabs--;
                    Log = $"12. <Конкатенация> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"11.1 <Конкатенация> == <Конкатенация> → FALSE";

            // 12.2 <Аргумент конкатенации>
            Log = $"12.2 <Аргумент конкатенации> →";
            // если посдевовательность удовлетворяет шаблону <Аргумент конкатенации>
            if (ParseConcatenationArgument(terminals))
            {
                Log = $"12.2 <Аргумент конкатенации> → TRUE";
                Tabs--;
                Log = $"12. <Конкатенация> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"12.2 <Аргумент конкатенации> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"12. <Конкатенация> → FALSE";
            Tabs--;
            return false;
        }

        /// 13. Аргумент конкатенации
        private static bool ParseConcatenationArgument(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"13. <Аргумент конкатенации> →";
            Tabs++;
            // 13.1 <Идентификатор>
            Log = $"13.1 <Идентификатор> →";
            // если посдевовательность удовлетворяет шаблону <Идентификатор>
            if (ParseIdentifier(terminals))
            {
                Log = $"13.1 <Идентификатор> → TRUE";
                Tabs--;
                Log = $"13. <Аргумент конкатенации> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"13.1 <Идентификатор> → FALSE";

            // 13.2 СТРОКА
            Log = $"13.2 СТРОКА →";
            // если в последователельности ровно один терминал
            if (terminals.Count == 1)
            {
                // если первый терминал булеан
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.TextLine)
                {
                    Log = $"13.2 СТРОКА → TRUE";
                    Tabs--;
                    Log = $"13. <Аргумент конкатенации> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"13.2 СТРОКА → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"13. <Аргумент конкатенации> → FALSE";
            Tabs--;
            return false;
        }

        /// 14. Числовое сравнение
        private static bool ParseNumericalComparison(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"14. <Числовое сравнение> →";
            Tabs++;
            // 14.1 <Сложение и вычитание> ОПЕРАТОР СРАВНЕНИЯ <Сложение и вычитание>
            Log = $"14.1 <Сложение и вычитание> ОПЕРАТОР СРАВНЕНИЯ <Сложение и вычитание> →";
            // берём все операторы сравнения
            ETerminalType[] comparsions =
                [ETerminalType.Greater,
                ETerminalType.Less,
                ETerminalType.Equal,
                ETerminalType.GreaterEqual,
                ETerminalType.LessEqual];
            // для каждого оператора сравнения
            foreach (var comparsionType in comparsions)
            {
                // находим индекс первого вхождения этого опрератора в последовательность
                int firstcomparsion = terminals.FindIndex(t => t.TerminalType == comparsionType);
                // если опрератор нашелся
                if (firstcomparsion != -1)
                {
                    // выделяем подпоследовательности для парсинга
                    var partForLeftAdditionAndSubtraction = terminals[..firstcomparsion];
                    var partForRightAdditionAndSubtraction = terminals[(firstcomparsion + 1)..];
                    // проверяем подпоследовательности
                    if (ParseAdditionAndSubtraction(partForLeftAdditionAndSubtraction) &&
                        ParseAdditionAndSubtraction(partForRightAdditionAndSubtraction))
                    {
                        Log = $"14.1 <Сложение и вычитание> ОПЕРАТОР СРАВНЕНИЯ <Сложение и вычитание> → TRUE";
                        Tabs--;
                        Log = $"14. <Числовое сравнение> → TRUE";
                        Tabs--;
                        return true;
                    }
                }
            }
            Log = $"14.1 <Сложение и вычитание> ОПЕРАТОР СРАВНЕНИЯ <Сложение и вычитание> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"14. <Числовое сравнение> → FALSE";
            Tabs--;
            return false;
        }

        /// 15. Оператор сравнения
        private static bool ParseComparisonOperator(List<Terminal> terminals)
        {
            throw new NotImplementedException();
        }

        /// 16. Сложение и вычитание
        private static bool ParseAdditionAndSubtraction(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"16. <Сложение и вычитание> →";
            Tabs++;
            // 16.1 <Умножение и деление> + <Сложение и вычитание>
            Log = $"16.1 <Умножение и деление> + <Сложение и вычитание> →";
            // находим индекс первого +
            int firstPlus = terminals.FindIndex(t => t.TerminalType == ETerminalType.Plus);
            // если + нашелся
            if (firstPlus != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForMultiplicationAndDivision = terminals[..firstPlus];
                var partForAdditionAndSubtraction = terminals[(firstPlus + 1)..];
                // проверяем подпоследовательности
                if (ParseMultiplicationAndDivision(partForMultiplicationAndDivision) &&
                    ParseAdditionAndSubtraction(partForAdditionAndSubtraction))
                {
                    Log = $"16.1 <Умножение и деление> + <Сложение и вычитание> → TRUE";
                    Tabs--;
                    Log = $"16. <Сложение и вычитание> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"16.1 <Умножение и деление> + <Сложение и вычитание> → FALSE";

            // 16.2 <Умножение и деление> - <Сложение и вычитание>
            Log = $"16.2 <Умножение и деление> - <Сложение и вычитание> →";
            // находим индекс первого -
            int firstMinus = terminals.FindIndex(t => t.TerminalType == ETerminalType.Minus);
            // если - нашелся
            if (firstMinus != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForMultiplicationAndDivision = terminals[..firstMinus];
                var partForAdditionAndSubtraction = terminals[(firstMinus + 1)..];
                // проверяем подпоследовательности
                if (ParseMultiplicationAndDivision(partForMultiplicationAndDivision) &&
                    ParseAdditionAndSubtraction(partForAdditionAndSubtraction))
                {
                    Log = $"16.2 <Умножение и деление> - <Сложение и вычитание> → TRUE";
                    Tabs--;
                    Log = $"16. <Сложение и вычитание> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"16.2 <Умножение и деление> - <Сложение и вычитание> → FALSE";

            // 16.3 <Умножение и деление>
            Log = $"16.3 <Умножение и деление> →";
            // если посдевовательность удовлетворяет шаблону <Умножение и деление>
            if (ParseMultiplicationAndDivision(terminals))
            {
                Log = $"16.3 <Умножение и деление> → TRUE";
                Tabs--;
                Log = $"16. <Сложение и вычитание> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"16.3 <Умножение и деление> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"16. <Сложение и вычитание> → FALSE";
            Tabs--;
            return false;
        }

        /// 17. Умножение и деление
        private static bool ParseMultiplicationAndDivision(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"17. <Умножение и деление> →";
            Tabs++;
            // 17.1 <Унарный минус> * <Умножение и деление>
            Log = $"17.1 <Унарный минус> * <Умножение и деление> →";
            // находим индекс первого *
            int firstMultiply = terminals.FindIndex(t => t.TerminalType == ETerminalType.Multiply);
            // если * нашелся
            if (firstMultiply != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForUnaryMinus = terminals[..firstMultiply];
                var partForMultiplicationAndDivision = terminals[(firstMultiply + 1)..];
                // проверяем подпоследовательности
                if (ParseUnaryMinus(partForUnaryMinus) &&
                    ParseMultiplicationAndDivision(partForMultiplicationAndDivision))
                {
                    Log = $"17.1 <Унарный минус> * <Умножение и деление> → TRUE";
                    Tabs--;
                    Log = $"17. <Умножение и деление> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"17.1 <Унарный минус> * <Умножение и деление> → FALSE";

            // 17.2 <Унарный минус> / <Умножение и деление> 
            Log = $"17.2 <Унарный минус> / <Умножение и деление> →";
            // находим индекс первого /
            int firstDivide = terminals.FindIndex(t => t.TerminalType == ETerminalType.Divide);
            // если / нашелся
            if (firstDivide != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForUnaryMinus = terminals[..firstDivide];
                var partForMultiplicationAndDivision = terminals[(firstDivide + 1)..];
                // проверяем подпоследовательности
                if (ParseUnaryMinus(partForUnaryMinus) &&
                    ParseMultiplicationAndDivision(partForMultiplicationAndDivision))
                {
                    Log = $"17.2 <Унарный минус> / <Умножение и деление> → TRUE";
                    Tabs--;
                    Log = $"17. <Умножение и деление> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"17.2 <Унарный минус> / <Умножение и деление> → FALSE";

            // 17.3 <Унарный минус> % <Умножение и деление>
            Log = $"17.3 <Унарный минус> % <Умножение и деление> →";
            // находим индекс первого %
            int firstModulus = terminals.FindIndex(t => t.TerminalType == ETerminalType.Modulus);
            // если % нашелся
            if (firstModulus != -1)
            {
                // выделяем подпоследовательности для парсинга
                var partForUnaryMinus = terminals[..firstModulus];
                var partForMultiplicationAndDivision = terminals[(firstModulus + 1)..];
                // проверяем подпоследовательности
                if (ParseUnaryMinus(partForUnaryMinus) &&
                    ParseMultiplicationAndDivision(partForMultiplicationAndDivision))
                {
                    Log = $"17.3 <Унарный минус> % <Умножение и деление> → TRUE";
                    Tabs--;
                    Log = $"17. <Умножение и деление> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"17.3 <Унарный минус> % <Умножение и деление> → FALSE";

            // 17.4 <Унарный минус>
            Log = $"17.4 <Унарный минус> →";
            // если посдевовательность удовлетворяет шаблону <Унарный минус>
            if (ParseUnaryMinus(terminals))
            {
                Log = $"17.4 <Унарный минус> → TRUE";
                Tabs--;
                Log = $"17. <Умножение и деление> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"17.4 <Унарный минус> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"17. <Умножение и деление> → FALSE";
            Tabs--;
            return false;
        }

        /// 18. Унарный минус
        private static bool ParseUnaryMinus(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"18. <Унарный минус> →";
            Tabs++;
            // 18.1 !<Аргумент унарного минуса>
            Log = $"18.1 !<Аргумент унарного минуса> →";
            // если первый терминал -
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Minus)
            {
                // выделяем подпоследовательность для парсинга
                var partForUnaryMinusArgument = terminals[1..];
                // проверяем подпоследовательность
                if (ParseUnaryMinusArgument(partForUnaryMinusArgument))
                {
                    Log = $"18.1 !<Аргумент унарного минуса> → TRUE";
                    Tabs--;
                    Log = $"18. <Унарный минус> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"18.1 !<Аргумент унарного минуса> → FALSE";

            // 18.2 <Аргумент унарного минуса>
            Log = $"18.2 <Аргумент унарного минуса> →";
            // если посдевовательность удовлетворяет шаблону <Аргумент унарного минуса>
            if (ParseUnaryMinusArgument(terminals))
            {
                Log = $"18.2 <Аргумент унарного минуса> → TRUE";
                Tabs--;
                Log = $"18. <Унарный минус> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"18.2 <Аргумент унарного минуса> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"18. <Унарный минус> → FALSE";
            Tabs--;
            return false;
        }

        /// 19. Аргумент унарного минуса
        private static bool ParseUnaryMinusArgument(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"19. <Аргумент унарного минуса> →";
            Tabs++;
            // 19.1 (<Сложение и вычитание>)
            Log = $"19.1 (<Сложение и вычитание>) →";
            // если первый терминал (
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.LeftParen)
            {
                // находим индекс парной )
                int rightParenIndex = FindPairedClosingBracket(0, terminals);
                // если парная ) успешно нашлась
                if (rightParenIndex != -1)
                {
                    // если парная ) это последний терминал
                    if (rightParenIndex == terminals.Count - 1)
                    {
                        // выделяем подпоследовательность для парсинга
                        var partForAdditionAndSubtraction = terminals[1..rightParenIndex];
                        // проверяем подпоследовательности
                        if (ParseAdditionAndSubtraction(partForAdditionAndSubtraction))
                        {
                            Log = $"19.1 (<Сложение и вычитание>) → TRUE";
                            Tabs--;
                            Log = $"19. <Аргумент унарного минуса> → TRUE";
                            Tabs--;
                            return true;
                        }
                    }
                }
            }
            Log = $"19.1 (<Сложение и вычитание>) → FALSE";

            // 19.2 <Идентификатор>
            Log = $"19.2 <Идентификатор> →";
            // если посдевовательность удовлетворяет шаблону <Идентификатор>
            if (ParseIdentifier(terminals))
            {
                Log = $"19.2 <Идентификатор> → TRUE";
                Tabs--;
                Log = $"19. <Аргумент унарного минуса> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"19.2 <Идентификатор> → FALSE";

            // 19.3 ЧИСЛО
            Log = $"19.3 ЧИСЛО →";
            // если в последователельности ровно один терминал
            if (terminals.Count == 1)
            {
                // если первый терминал число
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Number)
                {
                    Log = $"19.3 ЧИСЛО → TRUE";
                    Tabs--;
                    Log = $"19. <Аргумент унарного минуса> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"19.3 ЧИСЛО → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"19. <Аргумент унарного минуса> → FALSE";
            Tabs--;
            return false;
        }

        /// 20. Идентификатор
        private static bool ParseIdentifier(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"20. <Идентификатор> →";
            Tabs++;
            // 20.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>]
            Log = $"20.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>] →";
            // если первый терминал имяПеременной
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.VariableName)
            {
                // предполагаемый индекс [
                int leftBracketIndex = 1;
                // если по индексу действительно [
                if (terminals.ElementAtOrDefault(leftBracketIndex)?.TerminalType == ETerminalType.LeftBracket)
                {
                    // находим индекс парной ]
                    int rightBracketIndex = FindPairedClosingBracket(leftBracketIndex, terminals);
                    // если ] была успешно найдена
                    if (rightBracketIndex != -1)
                    {
                        // если парная ] это последний терминал
                        if (rightBracketIndex == terminals.Count - 1)
                        {
                            // выделяем подпоследовательность для парсинга
                            var partForIndexer = terminals[(leftBracketIndex + 1)..rightBracketIndex];
                            // проверяем подпоследовательности
                            if (ParseIndexer(partForIndexer))
                            {
                                Log = $"20.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>] → TRUE";
                                Tabs--;
                                Log = $"20. <Идентификатор> → TRUE";
                                Tabs--;
                                return true;
                            }
                        }
                    }
                }
            }
            Log = $"20.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>] → FALSE";

            // 20.2 НАЗВАНИЕ ПЕРЕМЕННОЙ
            Log = $"20.2 НАЗВАНИЕ ПЕРЕМЕННОЙ →";
            // если в последователельности ровно один терминал
            if (terminals.Count == 1)
            {
                // если первый название переменной
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.VariableName)
                {
                    Log = $"20.2 НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                    Tabs--;
                    Log = $"20. <Идентификатор> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"20.2 НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"20. <Идентификатор> → FALSE";
            Tabs--;
            return false;
        }

        /// 21. Индексатор
        private static bool ParseIndexer(List<Terminal> terminals)
        {
            Tabs++;
            Log = $"21. <Индексатор> →";
            Tabs++;
            // 21.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>]
            Log = $"21.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>] →";
            // если первый терминал имяПеременной
            if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.VariableName)
            {
                // предполагаемый индекс [
                int leftBracketIndex = 1;
                // если по индексу действительно [
                if (terminals.ElementAtOrDefault(leftBracketIndex)?.TerminalType == ETerminalType.LeftBracket)
                {
                    // находим индекс парной ]
                    int rightBracketIndex = FindPairedClosingBracket(leftBracketIndex, terminals);
                    // если ] была успешно найдена
                    if (rightBracketIndex != -1)
                    {
                        // если парная ] это последний терминал
                        if (rightBracketIndex == terminals.Count - 1)
                        {
                            // выделяем подпоследовательность для парсинга
                            var partForIndexer = terminals[(leftBracketIndex + 1)..rightBracketIndex];
                            // проверяем подпоследовательности
                            if (ParseIndexer(partForIndexer))
                            {
                                Log = $"21.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>] → TRUE";
                                Tabs--;
                                Log = $"21. <Индексатор> → TRUE";
                                Tabs--;
                                return true;
                            }
                        }
                    }
                }
            }
            Log = $"21.1 НАЗВАНИЕ ПЕРЕМЕННОЙ[<Индексатор>] → FALSE";

            // 21.2 НАЗВАНИЕ ПЕРЕМЕННОЙ
            Log = $"21.2 НАЗВАНИЕ ПЕРЕМЕННОЙ →";
            // если в последователельности ровно один терминал
            if (terminals.Count == 1)
            {
                // если первый терминал число
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.VariableName)
                {
                    Log = $"21.2 НАЗВАНИЕ ПЕРЕМЕННОЙ → TRUE";
                    Tabs--;
                    Log = $"21. <Индексатор> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"21.2 НАЗВАНИЕ ПЕРЕМЕННОЙ → FALSE";

            // 21.3 ЧИСЛО
            Log = $"21.3 ЧИСЛО →";
            // если в последователельности ровно один терминал
            if (terminals.Count == 1)
            {
                // если первый терминал число
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Number)
                {
                    Log = $"21.3 ЧИСЛО → TRUE";
                    Tabs--;
                    Log = $"21. <Индексатор> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"21.3 ЧИСЛО → FALSE";

            // 21.3 ЧИСЛО
            Log = $"21.3 ЧИСЛО →";
            // если в последователельности ровно один терминал
            if (terminals.Count == 1)
            {
                // если первый терминал число
                if (terminals.ElementAtOrDefault(0)?.TerminalType == ETerminalType.Number)
                {
                    Log = $"21.3 ЧИСЛО → TRUE";
                    Tabs--;
                    Log = $"21. <Индексатор> → TRUE";
                    Tabs--;
                    return true;
                }
            }
            Log = $"21.3 ЧИСЛО → FALSE";

            // 20.4 <Сложение и вычитание>
            Log = $"20.4 <Сложение и вычитание> →";
            // если посдевовательность удовлетворяет шаблону <Аргумент унарного минуса>
            if (ParseAdditionAndSubtraction(terminals))
            {
                Log = $"20.4 <Сложение и вычитание> → TRUE";
                Tabs--;
                Log = $"21. <Индексатор> → TRUE";
                Tabs--;
                return true;
            }
            Log = $"20.4 <Сложение и вычитание> → FALSE";

            // последовательность не подпадает ни под один из шаблонов
            Tabs--;
            Log = $"21. <Индексатор> → FALSE";
            Tabs--;
            return false;
        }
    }
}