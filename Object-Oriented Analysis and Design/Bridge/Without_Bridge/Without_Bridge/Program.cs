using System;
using System.IO;

public abstract class Document
{
    public string Content { get; set; }

    protected Document()
    {
        Content = "";
    }

    public abstract void ProcessToScreen();
    public abstract void ProcessToFile(string filePath);
}

public class TextDocument : Document
{
    public TextDocument() : base() { }

    public override void ProcessToScreen()
    {
        Content = "Это текстовый документ.";
        Console.WriteLine($"Вывод на экран:\n{Content}");
    }

    public override void ProcessToFile(string filePath)
    {
        Content = "Это текстовый документ.";
        try
        {
            File.WriteAllText(filePath, Content);
            Console.WriteLine($"Содержимое успешно сохранено в файл: {filePath}\n---");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Ошибка при сохранении в файл {filePath}: {ex.Message}");
        }
    }
}

public class PdfDocument : Document
{
    public PdfDocument() : base() { }

    public override void ProcessToScreen()
    {
        Content = "Это PDF-документ с форматированием.";
        Console.WriteLine($"Вывод на экран:\n{Content}");
    }

    public override void ProcessToFile(string filePath)
    {
        Content = "Это PDF-документ с форматированием.";
        try
        {
            File.WriteAllText(filePath, Content);
            Console.WriteLine($"Содержимое успешно сохранено в файл: {filePath}\n---");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Ошибка при сохранении в файл {filePath}: {ex.Message}");
        }
    }
}

class Program
{
    static void Main(string[] args)
    {
        Document textDocOnScreen = new TextDocument();
        Document textDocToFile = new TextDocument();
        Document pdfDocOnScreen = new PdfDocument();
        Document pdfDocToFile = new PdfDocument();

        textDocOnScreen.ProcessToScreen();
        textDocToFile.ProcessToFile("output.txt");
        pdfDocOnScreen.ProcessToScreen();
        pdfDocToFile.ProcessToFile("output.txt");
    }
}