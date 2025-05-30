using System;
using System.Collections.Generic;

namespace Компилятор
{
    // Статический класс, представляющий таблицу переходов для лексического анализатора.
    // Определяет, какое действие (метод анализа) необходимо выполнить 
    // в зависимости от текущей группы символов.
    public static class TransitionTable
    {
        // Словарь, хранящий соответствия между строковым представлением группы символов
        // и действием (методом лексического анализатора), которое должно быть выполнено.
        private static readonly Dictionary<string, Action> _transitions;

        // Статический конструктор, инициализирующий таблицу переходов.
        // Заполняет словарь `_transitions` парами "группа символов" - "действие".
        static TransitionTable()
        {
            _transitions = new Dictionary<string, Action>
            {
                // Цифры -> Анализ числа
                { "<ц>", LexicalAnalyzer.NUM_Analyse },
                // Буквы -> Анализ идентификатора/ключевого слова
                { "<б>", LexicalAnalyzer.ID_Analyse },
                // Пробельные символы -> Пропустить
                { "< >", () => LexicalAnalyzer.SkipWhitespace() },
                // Двойная кавычка -> Анализ строки
                { "<\">", LexicalAnalyzer.STR_Analyse },
                // Точка с запятой -> Простой токен
                { "<;>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.Semicolon) },
                // Плюс -> Простой токен
                { "<+>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.Plus) },
                // Минус -> Простой токен
                { "<->", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.Minus) },
                // Умножение -> Простой токен
                { "<*>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.Multiply) },
                // Деление -> Простой токен
                { "</>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.Divide) },
                // Процент (остаток от деления) -> Простой токен
                { "<%>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.Modulus) },
                // Меньше -> Анализ оператора '<' или '<='
                { "<<>", LexicalAnalyzer.LESS_Analyse },
                // Больше -> Анализ оператора '>' или '>='
                { "<>>", LexicalAnalyzer.MORE_Analyse },
                // Равно -> Анализ оператора '=' или '=='
                { "<=>", LexicalAnalyzer.EQUAL_Analyse },
                // Амперсанд -> Анализ оператора '&&'
                { "<&>", LexicalAnalyzer.AND_Analyse },
                // Вертикальная черта -> Анализ оператора '||'
                { "<|>", LexicalAnalyzer.OR_Analyse },
                // Восклицательный знак (логическое НЕ) -> Простой токен
                { "<!>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.Not) },
                // Открывающая круглая скобка -> Простой токен
                { "<(>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.LeftParen) },
                // Закрывающая круглая скобка -> Простой токен
                { "<)>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.RightParen) },
                // Открывающая квадратная скобка -> Простой токен
                { "<[>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.LeftBracket) },
                // Закрывающая квадратная скобка -> Простой токен
                { "<]>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.RightBracket) },
                // Открывающая фигурная скобка -> Простой токен
                { "<{>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.LeftBrace) },
                // Закрывающая фигурная скобка -> Простой токен
                { "<}>", () => LexicalAnalyzer.ProcessSimpleToken(ETerminalType.RightBrace) }
            };
        }

        // Пытается получить действие (метод анализа) из таблицы переходов 
        // для указанной группы символов.
        // charGroup: Строковое представление группы символов.
        // action: Выходной параметр, содержащий найденное действие, если оно существует.
        // returns: True, если действие для данной группы символов найдено, иначе False.
        public static bool TryGetAction(string charGroup, out Action action)
        {
            return _transitions.TryGetValue(charGroup, out action);
        }
    }
} 