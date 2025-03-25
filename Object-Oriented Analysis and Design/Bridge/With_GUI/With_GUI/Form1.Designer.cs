namespace BridgePatternApp
{
    partial class Form1
    {
        private System.ComponentModel.IContainer components = null;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.lblInput = new System.Windows.Forms.Label();
            this.txtInputPath = new System.Windows.Forms.TextBox();
            this.btnBrowseInput = new System.Windows.Forms.Button();
            this.grpOutputOptions = new System.Windows.Forms.GroupBox();
            this.rdoWordFile = new System.Windows.Forms.RadioButton();
            this.rdoScreen = new System.Windows.Forms.RadioButton();
            this.pnlWordFileOptions = new System.Windows.Forms.Panel();
            this.btnBrowseOutput = new System.Windows.Forms.Button();
            this.txtOutputPath = new System.Windows.Forms.TextBox();
            this.lblOutput = new System.Windows.Forms.Label();
            this.pnlDisplayArea = new System.Windows.Forms.Panel();
            this.pbImage = new System.Windows.Forms.PictureBox();
            this.rtbContent = new System.Windows.Forms.RichTextBox();
            this.btnProcess = new System.Windows.Forms.Button();
            this.lblStatus = new System.Windows.Forms.Label();
            this.grpOutputOptions.SuspendLayout();
            this.pnlWordFileOptions.SuspendLayout();
            this.pnlDisplayArea.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbImage)).BeginInit();
            this.SuspendLayout();

            // lblInput
            this.lblInput.AutoSize = true;
            this.lblInput.Location = new System.Drawing.Point(12, 15);
            this.lblInput.Name = "lblInput";
            this.lblInput.Size = new System.Drawing.Size(106, 13);
            this.lblInput.TabIndex = 0;
            this.lblInput.Text = "Выберите входной файл:";

            // txtInputPath
            this.txtInputPath.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
            | System.Windows.Forms.AnchorStyles.Right)));
            this.txtInputPath.Location = new System.Drawing.Point(124, 12);
            this.txtInputPath.Name = "txtInputPath";
            this.txtInputPath.Size = new System.Drawing.Size(300, 20);
            this.txtInputPath.TabIndex = 1;
            this.txtInputPath.ReadOnly = true;

            // btnBrowseInput
            this.btnBrowseInput.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnBrowseInput.Location = new System.Drawing.Point(430, 10);
            this.btnBrowseInput.Name = "btnBrowseInput";
            this.btnBrowseInput.Size = new System.Drawing.Size(75, 23);
            this.btnBrowseInput.TabIndex = 2;
            this.btnBrowseInput.Text = "Обзор...";
            this.btnBrowseInput.UseVisualStyleBackColor = true;
            this.btnBrowseInput.Click += new System.EventHandler(this.btnBrowseInput_Click);

            // grpOutputOptions
            this.grpOutputOptions.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
            | System.Windows.Forms.AnchorStyles.Right)));
            this.grpOutputOptions.Controls.Add(this.rdoWordFile);
            this.grpOutputOptions.Controls.Add(this.rdoScreen);
            this.grpOutputOptions.Location = new System.Drawing.Point(12, 40);
            this.grpOutputOptions.Name = "grpOutputOptions";
            this.grpOutputOptions.Size = new System.Drawing.Size(493, 60);
            this.grpOutputOptions.TabIndex = 3;
            this.grpOutputOptions.Text = "Варианты вывода";

            // rdoWordFile
            this.rdoWordFile.AutoSize = true;
            this.rdoWordFile.Location = new System.Drawing.Point(120, 25);
            this.rdoWordFile.Name = "rdoWordFile";
            this.rdoWordFile.Size = new System.Drawing.Size(95, 17);
            this.rdoWordFile.TabIndex = 1;
            this.rdoWordFile.Text = "В Word-файл";
            this.rdoWordFile.UseVisualStyleBackColor = true;
            this.rdoWordFile.CheckedChanged += new System.EventHandler(this.rdoOutput_CheckedChanged);

            // rdoScreen
            this.rdoScreen.AutoSize = true;
            this.rdoScreen.Checked = true;
            this.rdoScreen.Location = new System.Drawing.Point(15, 25);
            this.rdoScreen.Name = "rdoScreen";
            this.rdoScreen.Size = new System.Drawing.Size(85, 17);
            this.rdoScreen.TabIndex = 0;
            this.rdoScreen.TabStop = true;
            this.rdoScreen.Text = "На экран";
            this.rdoScreen.UseVisualStyleBackColor = true;
            this.rdoScreen.CheckedChanged += new System.EventHandler(this.rdoOutput_CheckedChanged);

            // pnlWordFileOptions
            this.pnlWordFileOptions.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
            | System.Windows.Forms.AnchorStyles.Right)));
            this.pnlWordFileOptions.Controls.Add(this.btnBrowseOutput);
            this.pnlWordFileOptions.Controls.Add(this.txtOutputPath);
            this.pnlWordFileOptions.Controls.Add(this.lblOutput);
            this.pnlWordFileOptions.Location = new System.Drawing.Point(12, 106);
            this.pnlWordFileOptions.Name = "pnlWordFileOptions";
            this.pnlWordFileOptions.Size = new System.Drawing.Size(493, 40);
            this.pnlWordFileOptions.TabIndex = 4;
            this.pnlWordFileOptions.Visible = false;

            // btnBrowseOutput
            this.btnBrowseOutput.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnBrowseOutput.Location = new System.Drawing.Point(430, 8);
            this.btnBrowseOutput.Name = "btnBrowseOutput";
            this.btnBrowseOutput.Size = new System.Drawing.Size(75, 23);
            this.btnBrowseOutput.TabIndex = 2;
            this.btnBrowseOutput.Text = "Обзор...";
            this.btnBrowseOutput.UseVisualStyleBackColor = true;
            this.btnBrowseOutput.Click += new System.EventHandler(this.btnBrowseOutput_Click);

            // txtOutputPath
            this.txtOutputPath.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
            | System.Windows.Forms.AnchorStyles.Right)));
            this.txtOutputPath.Location = new System.Drawing.Point(124, 10);
            this.txtOutputPath.Name = "txtOutputPath";
            this.txtOutputPath.Size = new System.Drawing.Size(300, 20);
            this.txtOutputPath.TabIndex = 1;
            this.txtOutputPath.ReadOnly = true;

            // lblOutput
            this.lblOutput.AutoSize = true;
            this.lblOutput.Location = new System.Drawing.Point(3, 13);
            this.lblOutput.Name = "lblOutput";
            this.lblOutput.Size = new System.Drawing.Size(115, 13);
            this.lblOutput.TabIndex = 0;
            this.lblOutput.Text = "Путь для сохранения:";

            // pnlDisplayArea
            this.pnlDisplayArea.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
            | System.Windows.Forms.AnchorStyles.Left)
            | System.Windows.Forms.AnchorStyles.Right)));
            this.pnlDisplayArea.Controls.Add(this.pbImage);
            this.pnlDisplayArea.Controls.Add(this.rtbContent);
            this.pnlDisplayArea.Location = new System.Drawing.Point(12, 152);
            this.pnlDisplayArea.Name = "pnlDisplayArea";
            this.pnlDisplayArea.Size = new System.Drawing.Size(493, 300);
            this.pnlDisplayArea.TabIndex = 5;

            // pbImage
            this.pbImage.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
            | System.Windows.Forms.AnchorStyles.Right)));
            this.pbImage.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.pbImage.Location = new System.Drawing.Point(3, 159);
            this.pbImage.Name = "pbImage";
            this.pbImage.Size = new System.Drawing.Size(487, 138);
            this.pbImage.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.pbImage.TabIndex = 1;
            this.pbImage.TabStop = false;

            // rtbContent
            this.rtbContent.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
            | System.Windows.Forms.AnchorStyles.Right)));
            this.rtbContent.Location = new System.Drawing.Point(3, 3);
            this.rtbContent.Name = "rtbContent";
            this.rtbContent.Size = new System.Drawing.Size(487, 150);
            this.rtbContent.TabIndex = 0;
            this.rtbContent.Text = "";

            // btnProcess
            this.btnProcess.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnProcess.Location = new System.Drawing.Point(430, 458);
            this.btnProcess.Name = "btnProcess";
            this.btnProcess.Size = new System.Drawing.Size(75, 23);
            this.btnProcess.TabIndex = 6;
            this.btnProcess.Text = "Обработать";
            this.btnProcess.UseVisualStyleBackColor = true;
            this.btnProcess.Click += new System.EventHandler(this.btnProcess_Click);

            // lblStatus
            this.lblStatus.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.lblStatus.AutoSize = true;
            this.lblStatus.Location = new System.Drawing.Point(12, 463);
            this.lblStatus.Name = "lblStatus";
            this.lblStatus.Size = new System.Drawing.Size(0, 13);
            this.lblStatus.TabIndex = 7;

            // Form1
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(517, 493);
            this.Controls.Add(this.lblStatus);
            this.Controls.Add(this.btnProcess);
            this.Controls.Add(this.pnlDisplayArea);
            this.Controls.Add(this.pnlWordFileOptions);
            this.Controls.Add(this.grpOutputOptions);
            this.Controls.Add(this.btnBrowseInput);
            this.Controls.Add(this.txtInputPath);
            this.Controls.Add(this.lblInput);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.MaximizeBox = false;
            this.Name = "Form1";
            this.Text = "Обработчик документов (паттерн Мост)";
            this.grpOutputOptions.ResumeLayout(false);
            this.grpOutputOptions.PerformLayout();
            this.pnlWordFileOptions.ResumeLayout(false);
            this.pnlWordFileOptions.PerformLayout();
            this.pnlDisplayArea.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.pbImage)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();
        }

        private System.Windows.Forms.Label lblInput;
        private System.Windows.Forms.TextBox txtInputPath;
        private System.Windows.Forms.Button btnBrowseInput;
        private System.Windows.Forms.GroupBox grpOutputOptions;
        private System.Windows.Forms.RadioButton rdoWordFile;
        private System.Windows.Forms.RadioButton rdoScreen;
        private System.Windows.Forms.Panel pnlWordFileOptions;
        private System.Windows.Forms.Button btnBrowseOutput;
        private System.Windows.Forms.TextBox txtOutputPath;
        private System.Windows.Forms.Label lblOutput;
        private System.Windows.Forms.Panel pnlDisplayArea;
        private System.Windows.Forms.PictureBox pbImage;
        private System.Windows.Forms.RichTextBox rtbContent;
        private System.Windows.Forms.Button btnProcess;
        private System.Windows.Forms.Label lblStatus;
    }
}