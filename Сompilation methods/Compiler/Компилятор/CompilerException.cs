namespace Компилятор
{
    /// <summary>
    /// Исключение, возникающее при ошибках компиляции.
    /// Содержит информацию о месте возникновения ошибки в исходном коде.
    /// </summary>
    public class CompilerException : Exception
    {
        /// <summary>
        /// Номер строки, где произошла ошибка
        /// </summary>
        public int LineNumber { get; }

        /// <summary>
        /// Позиция символа в строке, где произошла ошибка
        /// </summary>
        public int CharPosition { get; }

        /// <summary>
        /// Создает новый экземпляр исключения компилятора
        /// </summary>
        /// <param name="message">Сообщение об ошибке</param>
        /// <param name="lineNumber">Номер строки</param>
        /// <param name="charPosition">Позиция символа в строке</param>
        public CompilerException(string message, int lineNumber, int charPosition)
            : base(message)
        {
            LineNumber = lineNumber;
            CharPosition = charPosition;
        }
    }
} 