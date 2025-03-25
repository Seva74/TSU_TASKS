using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas.Parser;
using iText.Kernel.Pdf.Canvas.Parser.Data;
using iText.Kernel.Pdf.Canvas.Parser.Listener;
using Xceed.Words.NET;

namespace BridgePatternApp
{
    // Интерфейс рендера (реализация в паттерне "Мост")
    public interface IDocumentRenderer
    {
        void Render(string content, Image? image = null);
    }

    // Рендер на экран через форму
    public class ScreenRenderer : IDocumentRenderer
    {
        private readonly Form1 form;

        public ScreenRenderer(Form1 form)
        {
            this.form = form ?? throw new ArgumentNullException(nameof(form));
        }

        public void Render(string content, Image? image = null)
        {
            form.DisplayContent(content, image);
        }
    }

    // Рендер в Word-файл
    public class WordFileRenderer : IDocumentRenderer
    {
        private readonly string filePath;

        public WordFileRenderer(string path)
        {
            filePath = path ?? throw new ArgumentNullException(nameof(path));
        }

        public void Render(string content, Image? image = null)
        {
            using (DocX doc = DocX.Create(filePath))
            {
                doc.InsertParagraph(content ?? string.Empty);
                if (image != null)
                {
                    using (MemoryStream ms = new MemoryStream())
                    {
                        image.Save(ms, System.Drawing.Imaging.ImageFormat.Jpeg);
                        ms.Position = 0;
                        var picture = doc.AddImage(ms);
                        doc.InsertParagraph().InsertPicture(picture.CreatePicture());
                    }
                }
                doc.Save();
            }
        }
    }

    // Абстрактный класс документа (абстракция в паттерне "Мост")
    public abstract class DocumentProcessor
    {
        protected IDocumentRenderer renderer;
        public string FilePath { get; }

        protected DocumentProcessor(IDocumentRenderer renderer, string filePath)
        {
            this.renderer = renderer ?? throw new ArgumentNullException(nameof(renderer));
            FilePath = !string.IsNullOrWhiteSpace(filePath) ? filePath : throw new ArgumentException("Путь к файлу не может быть пустым.", nameof(filePath));
        }

        public abstract void Process();
    }

    // Обработка текстового документа
    public class TextDocumentProcessor : DocumentProcessor
    {
        public TextDocumentProcessor(IDocumentRenderer renderer, string filePath) : base(renderer, filePath) { }

        public override void Process()
        {
            string content = File.ReadAllText(FilePath);
            renderer.Render(content);
        }
    }

    // Обработка PDF-документа
    public class PdfDocumentProcessor : DocumentProcessor
    {
        public PdfDocumentProcessor(IDocumentRenderer renderer, string filePath) : base(renderer, filePath) { }

        public override void Process()
        {
            string content = string.Empty;
            Image? image = null;

            try
            {
                using (PdfDocument pdf = new PdfDocument(new PdfReader(FilePath)))
                {
                    for (int i = 1; i <= pdf.GetNumberOfPages(); i++)
                    {
                        var page = pdf.GetPage(i);
                        content += PdfTextExtractor.GetTextFromPage(page);

                        if (image == null)
                        {
                            ImageExtractingListener listener = new ImageExtractingListener();
                            PdfCanvasProcessor processor = new PdfCanvasProcessor(listener);
                            processor.ProcessPageContent(page);
                            image = listener.GetFirstImage();
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                throw new InvalidOperationException($"Ошибка обработки PDF: {ex.Message}", ex);
            }

            renderer.Render(content, image);
        }
    }

    // Класс для извлечения изображений из PDF
    public class ImageExtractingListener : IEventListener
    {
        private readonly List<Image> images = new List<Image>();

        public void EventOccurred(IEventData data, EventType type)
        {
            if (type == EventType.RENDER_IMAGE)
            {
                if (data is ImageRenderInfo renderInfo)
                {
                    try
                    {
                        byte[] bytes = renderInfo.GetImage().GetImageBytes();
                        Image img = Image.FromStream(new MemoryStream(bytes));
                        images.Add(img);
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine($"Ошибка извлечения изображения: {ex.Message}");
                    }
                }
            }
        }

        public ICollection<EventType> GetSupportedEvents()
        {
            return new HashSet<EventType> { EventType.RENDER_IMAGE };
        }

        public Image? GetFirstImage()
        {
            return images.Count > 0 ? images[0] : null;
        }
    }

    static class Program
    {
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
        }
    }
}