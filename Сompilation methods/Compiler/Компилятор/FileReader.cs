namespace Компилятор
{
    /// <summary>
    /// Статический класс для считывания текста из файла
    /// </summary>
    public static class FileReader
    {
        /// <summary>
        /// Считывает текст из .txt файла с указанным именем в папке с программой
        /// </summary>
        public static string Read(string filePath)
        {
            // Считываем текст из файла
            string text = File.ReadAllText(filePath);

            // Удаляем все пробелы, переносы строк и символы табуляции
            string parsedText = RemoveWhitespace(text);
            return parsedText;
        }

        static string RemoveWhitespace(string input)
        {
            // Используем метод Replace для удаления нежелательных символов
            return input.Replace("\t", "")
                        .Replace("\r", "");
        }
    }
}
