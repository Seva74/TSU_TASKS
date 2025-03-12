using System;
using System.IO;
using System.Windows.Forms;

namespace DocumentProcessor
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void btnProcess_Click(object sender, EventArgs e)
        {
            IDocumentRenderer renderer;
            if (cbRenderType.SelectedItem.ToString() == "Экран")
            {
                renderer = new ScreenRenderer(txtOutput);
            }
            else
            {
                renderer = new FileRenderer("output.txt");
            }

            Document document;
            if (cbDocumentType.SelectedItem.ToString() == "Текстовый")
            {
                document = new TextDocument(renderer);
            }
            else
            {
                document = new PdfDocument(renderer);
            }

            document.Process();
        }
    }

    public interface IDocumentRenderer
    {
        void Render(string content);
    }

    // Конкретные реализаторы
    public class ScreenRenderer : IDocumentRenderer
    {
        private readonly TextBox outputBox;

        public ScreenRenderer(TextBox box)
        {
            outputBox = box;
        }

        public void Render(string content)
        {
            outputBox.Clear(); // Очищаем текстовое поле перед новым выводом
            outputBox.Text = $"Вывод на экран:\n{content}\n---\n";
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
                MessageBox.Show($"Содержимое успешно сохранено в файл: {filePath}");
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Ошибка при сохранении в файл {filePath}: {ex.Message}");
            }
        }
    }

    // Абстракция
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

    // Уточнённые абстракции
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
}