using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace BridgePatternApp
{
    public partial class Form1 : Form
    {
        private readonly ToolTip toolTip;

        public Form1()
        {
            InitializeComponent();
            toolTip = new ToolTip();
            SetupToolTips();
            UpdatePanelVisibility();
        }

        public void DisplayContent(string content, Image? image)
        {
            rtbContent.Clear();
            rtbContent.Text = content ?? string.Empty;
            pbImage.Image = image; // Автоматически очищается, если image == null
        }

        private void SetupToolTips()
        {
            toolTip.SetToolTip(btnBrowseInput, "Выберите текстовый файл (.txt) или PDF (.pdf)");
            toolTip.SetToolTip(btnBrowseOutput, "Укажите путь для сохранения Word-файла");
            toolTip.SetToolTip(btnProcess, "Нажмите для обработки выбранного файла");
        }

        private void UpdatePanelVisibility()
        {
            pnlWordFileOptions.Visible = rdoWordFile.Checked;
            pnlDisplayArea.Visible = rdoScreen.Checked;
        }

        private void btnBrowseInput_Click(object sender, EventArgs e)
        {
            using (OpenFileDialog openDialog = new OpenFileDialog())
            {
                openDialog.Filter = "Text Files (*.txt)|*.txt|PDF Files (*.pdf)|*.pdf";
                openDialog.Title = "Выберите входной файл";
                if (openDialog.ShowDialog() == DialogResult.OK)
                {
                    txtInputPath.Text = openDialog.FileName;
                    lblStatus.Text = "Файл выбран.";
                }
            }
        }

        private void btnBrowseOutput_Click(object sender, EventArgs e)
        {
            using (SaveFileDialog saveDialog = new SaveFileDialog())
            {
                saveDialog.Filter = "Word Documents (*.docx)|*.docx";
                saveDialog.DefaultExt = "docx";
                saveDialog.Title = "Выберите место сохранения Word-файла";
                if (saveDialog.ShowDialog() == DialogResult.OK)
                {
                    txtOutputPath.Text = saveDialog.FileName;
                    lblStatus.Text = "Путь сохранения выбран.";
                }
            }
        }

        private void rdoOutput_CheckedChanged(object sender, EventArgs e)
        {
            UpdatePanelVisibility();
        }

        private void btnProcess_Click(object sender, EventArgs e)
        {
            lblStatus.Text = string.Empty;

            if (string.IsNullOrWhiteSpace(txtInputPath.Text) || !File.Exists(txtInputPath.Text))
            {
                lblStatus.Text = "Ошибка: выберите существующий входной файл.";
                return;
            }

            string extension = Path.GetExtension(txtInputPath.Text).ToLower();
            if (extension != ".txt" && extension != ".pdf")
            {
                lblStatus.Text = "Ошибка: поддерживаются только .txt и .pdf.";
                return;
            }

            IDocumentRenderer renderer;
            if (rdoScreen.Checked)
            {
                renderer = new ScreenRenderer(this);
            }
            else
            {
                if (string.IsNullOrWhiteSpace(txtOutputPath.Text))
                {
                    lblStatus.Text = "Ошибка: укажите путь для сохранения Word-файла.";
                    return;
                }
                renderer = new WordFileRenderer(txtOutputPath.Text);
            }

            DocumentProcessor processor = extension == ".txt"
                ? new TextDocumentProcessor(renderer, txtInputPath.Text)
                : new PdfDocumentProcessor(renderer, txtInputPath.Text);

            try
            {
                processor.Process();
                lblStatus.Text = rdoScreen.Checked
                    ? "Файл успешно отображён на экране."
                    : "Файл успешно сохранён в Word.";
            }
            catch (Exception ex)
            {
                lblStatus.Text = $"Ошибка обработки: {ex.Message}";
            }
        }
    }
}