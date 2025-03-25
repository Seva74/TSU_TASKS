using System;
using System.IO;
using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas.Parser;

public interface IDocumentRenderer
{
    void Render(string content);
}

public class ScreenRenderer : IDocumentRenderer
{
    public void Render(string content)
    {
        Console.WriteLine($"Вывод на экран:\n{content}\n---");
    }
}

public class FileRenderer : IDocumentRenderer
{
    private readonly string filePath;

    public FileRenderer(string path)
    {
        filePath = path;
    }

    public void Render(string content)
    {
        File.WriteAllText(filePath, content);
        Console.WriteLine($"Содержимое успешно сохранено в файл: {filePath}\n---");
    }
}

public abstract class DocumentProcessor
{
    protected IDocumentRenderer renderer;
    public string FilePath { get; }

    protected DocumentProcessor(IDocumentRenderer renderer, string filePath)
    {
        this.renderer = renderer;
        FilePath = filePath;
    }

    public abstract void Process();
}

public class TextDocumentProcessor : DocumentProcessor
{
    public TextDocumentProcessor(IDocumentRenderer renderer, string filePath) : base(renderer, filePath) { }

    public override void Process()
    {
        string content = File.ReadAllText(FilePath);
        renderer.Render(content);
    }
}

public class PdfDocumentProcessor : DocumentProcessor
{
    public PdfDocumentProcessor(IDocumentRenderer renderer, string filePath) : base(renderer, filePath) { }

    public override void Process()
    {
        string content = string.Empty;
        using (PdfDocument pdf = new PdfDocument(new PdfReader(FilePath)))
        {
            for (int i = 1; i <= pdf.GetNumberOfPages(); i++)
            {
                content += PdfTextExtractor.GetTextFromPage(pdf.GetPage(i));
            }
        }
        renderer.Render(content);
    }
}

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Программа обработки документов (паттерн 'Мост')");
        Console.WriteLine("Поддерживаются файлы: .txt и .pdf\n");

        Console.Write("Введите путь к входному файлу (.txt или .pdf): ");
        string inputPath = Console.ReadLine();

        string extension = Path.GetExtension(inputPath).ToLower();

        Console.WriteLine("\nВыберите режим вывода:");
        Console.WriteLine("1 - Вывод на экран");
        Console.WriteLine("2 - Сохранение в файл");
        Console.Write("Введите номер (1 или 2): ");
        string choice = Console.ReadLine();

        IDocumentRenderer renderer;
        if (choice == "1")
        {
            renderer = new ScreenRenderer();
        }
        else
        {
            Console.Write("Введите путь для сохранения файла (например, output.txt): ");
            string outputPath = Console.ReadLine();
            renderer = new FileRenderer(outputPath);
        }

        DocumentProcessor processor = extension == ".txt"
            ? new TextDocumentProcessor(renderer, inputPath)
            : new PdfDocumentProcessor(renderer, inputPath);

        Console.WriteLine("\nОбработка файла...");
        processor.Process();
    }
}