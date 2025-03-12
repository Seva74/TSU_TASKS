namespace DocumentProcessor
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
            cbDocumentType = new ComboBox();
            cbRenderType = new ComboBox();
            btnProcess = new Button();
            txtOutput = new TextBox();
            SuspendLayout();
            // 
            // cbDocumentType
            // 
            cbDocumentType.FormattingEnabled = true;
            cbDocumentType.Items.AddRange(new object[] { "Текстовый", "PDF" });
            cbDocumentType.Location = new Point(33, 38);
            cbDocumentType.Margin = new Padding(5, 6, 5, 6);
            cbDocumentType.Name = "cbDocumentType";
            cbDocumentType.Size = new Size(247, 33);
            cbDocumentType.TabIndex = 0;
            // 
            // cbRenderType
            // 
            cbRenderType.FormattingEnabled = true;
            cbRenderType.Items.AddRange(new object[] { "Экран", "Файл" });
            cbRenderType.Location = new Point(33, 115);
            cbRenderType.Margin = new Padding(5, 6, 5, 6);
            cbRenderType.Name = "cbRenderType";
            cbRenderType.Size = new Size(247, 33);
            cbRenderType.TabIndex = 1;
            // 
            // btnProcess
            // 
            btnProcess.Location = new Point(33, 192);
            btnProcess.Margin = new Padding(5, 6, 5, 6);
            btnProcess.Name = "btnProcess";
            btnProcess.Size = new Size(167, 44);
            btnProcess.TabIndex = 2;
            btnProcess.Text = "Обработать";
            btnProcess.UseVisualStyleBackColor = true;
            btnProcess.Click += btnProcess_Click;
            // 
            // txtOutput
            // 
            txtOutput.Location = new Point(33, 269);
            txtOutput.Margin = new Padding(5, 6, 5, 6);
            txtOutput.Multiline = true;
            txtOutput.Name = "txtOutput";
            txtOutput.ReadOnly = true;
            txtOutput.Size = new Size(509, 227);
            txtOutput.TabIndex = 3;
            // 
            // Form1
            // 
            AutoScaleDimensions = new SizeF(10F, 25F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(716, 577);
            Controls.Add(txtOutput);
            Controls.Add(btnProcess);
            Controls.Add(cbRenderType);
            Controls.Add(cbDocumentType);
            Margin = new Padding(5, 6, 5, 6);
            Name = "Form1";
            Text = "Обработка документов";
            ResumeLayout(false);
            PerformLayout();
        }

        private System.Windows.Forms.ComboBox cbDocumentType;
        private System.Windows.Forms.ComboBox cbRenderType;
        private System.Windows.Forms.Button btnProcess;
        private System.Windows.Forms.TextBox txtOutput;
    }
}