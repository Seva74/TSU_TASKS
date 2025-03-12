using System;
using System.IO;

public interface IDocumentRenderer
{
    void Render(string content);
}

// Конкретные реализаторы
public class ScreenRenderer : IDocumentRenderer
{
    public void Render(string content)
    {
        Console.WriteLine($"Вывод на экран:\n{content}");
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
        try
        {
            File.WriteAllText(filePath, content);
            Console.WriteLine($"Содержимое успешно сохранено в файл: {filePath}\n---");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Ошибка при сохранении в файл {filePath}: {ex.Message}");
        }
    }
}
public abstract class Document
{
    protected IDocumentRenderer renderer;

    public string Content { get; set; }

    protected Document(IDocumentRenderer renderer)
    {
        this.renderer = renderer;
        Content = "";
    }

    public abstract void Process();
}

// Уточнённая абстракция
public class TextDocument : Document
{
    public TextDocument(IDocumentRenderer renderer) : base(renderer) { }

    public override void Process()
    {
        Content = "Это текстовый документ.";
        renderer.Render(Content);
    }
}
public class PdfDocument : Document
{
    public PdfDocument(IDocumentRenderer renderer) : base(renderer) { }

    public override void Process()
    {
        Content = "Это PDF-документ с форматированием.";
        renderer.Render(Content);
    }
}
class Program
{
    static void Main(string[] args)
    {
        IDocumentRenderer screenRenderer = new ScreenRenderer();
        IDocumentRenderer fileRenderer = new FileRenderer("output.txt");

        Document textDocOnScreen = new TextDocument(screenRenderer);
        Document textDocToFile = new TextDocument(fileRenderer);
        Document pdfDocOnScreen = new PdfDocument(screenRenderer);
        Document pdfDocToFile = new PdfDocument(fileRenderer);

        Console.WriteLine("Обработка документов:");
        textDocOnScreen.Process();
        textDocToFile.Process();
        pdfDocOnScreen.Process();
        pdfDocToFile.Process();
    }
}