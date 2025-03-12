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
            if (cbRenderType.SelectedItem.ToString() == "�����")
            {
                renderer = new ScreenRenderer(txtOutput);
            }
            else
            {
                renderer = new FileRenderer("output.txt");
            }

            Document document;
            if (cbDocumentType.SelectedItem.ToString() == "���������")
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

    // ���������� �����������
    public class ScreenRenderer : IDocumentRenderer
    {
        private readonly TextBox outputBox;

        public ScreenRenderer(TextBox box)
        {
            outputBox = box;
        }

        public void Render(string content)
        {
            outputBox.Clear(); // ������� ��������� ���� ����� ����� �������
            outputBox.Text = $"����� �� �����:\n{content}\n---\n";
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
                MessageBox.Show($"���������� ������� ��������� � ����: {filePath}");
            }
            catch (Exception ex)
            {
                MessageBox.Show($"������ ��� ���������� � ���� {filePath}: {ex.Message}");
            }
        }
    }

    // ����������
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

    // ��������� ����������
    public class TextDocument : Document
    {
        public TextDocument(IDocumentRenderer renderer) : base(renderer) { }

        public override void Process()
        {
            Content = "��� ��������� ��������.";
            renderer.Render(Content);
        }
    }

    public class PdfDocument : Document
    {
        public PdfDocument(IDocumentRenderer renderer) : base(renderer) { }

        public override void Process()
        {
            Content = "��� PDF-�������� � ���������������.";
            renderer.Render(Content);
        }
    }
}