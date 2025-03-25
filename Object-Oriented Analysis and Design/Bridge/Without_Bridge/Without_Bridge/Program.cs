using System;
using System.IO;
using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas.Parser;

public class TextDocument
{
    public string FilePath;

    public TextDocument(string filePath)
    {
        FilePath = filePath;
    }

    public void ProcessToScreen()
    {
        string content = File.ReadAllText(FilePath);
        Console.WriteLine($"Вывод на экран:\n{content}\n---");
    }

    public void ProcessToFile(string outputPath)
    {
        string content = File.ReadAllText(FilePath);
        File.WriteAllText(outputPath, content);
        Console.WriteLine($"Содержимое успешно сохранено в файл: {outputPath}\n---");
    }
}

public class PdfDocument
{
    public string FilePath;

    public PdfDocument(string filePath)
    {
        FilePath = filePath;
    }

    public void ProcessToScreen()
    {
        string content = string.Empty;
        using (var pdfReader = new PdfReader(FilePath))
        using (var pdfDoc = new iText.Kernel.Pdf.PdfDocument(pdfReader))
        {
            for (int i = 1; i <= pdfDoc.GetNumberOfPages(); i++)
            {
                content += PdfTextExtractor.GetTextFromPage(pdfDoc.GetPage(i));
            }
        }
        Console.WriteLine($"Вывод на экран:\n{content}\n---");
    }

    public void ProcessToFile(string outputPath)
    {
        string content = string.Empty;
        using (var pdfReader = new PdfReader(FilePath))
        using (var pdfDoc = new iText.Kernel.Pdf.PdfDocument(pdfReader))
        {
            for (int i = 1; i <= pdfDoc.GetNumberOfPages(); i++)
            {
                content += PdfTextExtractor.GetTextFromPage(pdfDoc.GetPage(i));
            }
        }
        File.WriteAllText(outputPath, content);
        Console.WriteLine($"Содержимое успешно сохранено в файл: {outputPath}\n---");
    }
}

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Программа обработки документов");
        Console.WriteLine("Поддерживаются файлы: .txt и .pdf\n");

        Console.Write("Введите путь к входному файлу (.txt или .pdf): ");
        string inputPath = Console.ReadLine()!;

        string extension = Path.GetExtension(inputPath).ToLower();

        Console.WriteLine("\nВыберите режим вывода:");
        Console.WriteLine("1 - Вывод на экран");
        Console.WriteLine("2 - Сохранение в файл");
        Console.Write("Введите номер (1 или 2): ");
        string choice = Console.ReadLine()!;

        if (extension == ".txt")
        {
            TextDocument textDoc = new TextDocument(inputPath);
            if (choice == "1")
            {
                Console.WriteLine("\nОбработка файла...");
                textDoc.ProcessToScreen();
            }
            else
            {
                Console.Write("Введите путь для сохранения файла (например, output.txt): ");
                string outputPath = Console.ReadLine()!;
                Console.WriteLine("\nОбработка файла...");
                textDoc.ProcessToFile(outputPath);
            }
        }
        else
        {
            PdfDocument pdfDoc = new PdfDocument(inputPath);
            if (choice == "1")
            {
                Console.WriteLine("\nОбработка файла...");
                pdfDoc.ProcessToScreen();
            }
            else
            {
                Console.Write("Введите путь для сохранения файла (например, output.txt): ");
                string outputPath = Console.ReadLine()!;
                Console.WriteLine("\nОбработка файла...");
                pdfDoc.ProcessToFile(outputPath);
            }
        }

        Console.WriteLine("Нажмите любую клавишу для выхода...");
        Console.ReadKey();
    }
}