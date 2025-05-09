namespace NormalRVSimulation
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
            System.Windows.Forms.DataVisualization.Charting.ChartArea chartArea1 = new System.Windows.Forms.DataVisualization.Charting.ChartArea();
            System.Windows.Forms.DataVisualization.Charting.Legend legend1 = new System.Windows.Forms.DataVisualization.Charting.Legend();
            this.cmbN = new System.Windows.Forms.ComboBox();
            this.btnGenerate = new System.Windows.Forms.Button();
            this.chartHistogram = new System.Windows.Forms.DataVisualization.Charting.Chart();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.lblSampleMeanValue = new System.Windows.Forms.Label();
            this.lblRelErrorMeanValue = new System.Windows.Forms.Label();
            this.lblSampleVarianceValue = new System.Windows.Forms.Label();
            this.lblRelErrorVarianceValue = new System.Windows.Forms.Label();
            this.lblChiSquareValue = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.chartHistogram)).BeginInit();
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();

            this.cmbN.FormattingEnabled = true;
            this.cmbN.Location = new System.Drawing.Point(12, 12);
            this.cmbN.Name = "cmbN";
            this.cmbN.Size = new System.Drawing.Size(100, 21);
            this.cmbN.TabIndex = 0;

            this.btnGenerate.Location = new System.Drawing.Point(118, 12);
            this.btnGenerate.Name = "btnGenerate";
            this.btnGenerate.Size = new System.Drawing.Size(100, 23);
            this.btnGenerate.TabIndex = 1;
            this.btnGenerate.Text = "Сгенерировать";
            this.btnGenerate.UseVisualStyleBackColor = true;
            this.btnGenerate.Click += new System.EventHandler(this.btnGenerate_Click);

            chartArea1.Name = "ChartArea1";
            this.chartHistogram.ChartAreas.Add(chartArea1);
            legend1.Name = "Legend1";
            this.chartHistogram.Legends.Add(legend1);
            this.chartHistogram.Location = new System.Drawing.Point(12, 41);
            this.chartHistogram.Name = "chartHistogram";
            this.chartHistogram.Size = new System.Drawing.Size(600, 300);
            this.chartHistogram.TabIndex = 2;
            this.chartHistogram.Text = "chartHistogram";

            this.tableLayoutPanel1.ColumnCount = 2;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 70F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 30F));
            this.tableLayoutPanel1.Controls.Add(this.label1, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.label2, 0, 1);
            this.tableLayoutPanel1.Controls.Add(this.label3, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.label4, 0, 3);
            this.tableLayoutPanel1.Controls.Add(this.label5, 0, 4);
            this.tableLayoutPanel1.Controls.Add(this.lblSampleMeanValue, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.lblRelErrorMeanValue, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.lblSampleVarianceValue, 1, 2);
            this.tableLayoutPanel1.Controls.Add(this.lblRelErrorVarianceValue, 1, 3);
            this.tableLayoutPanel1.Controls.Add(this.lblChiSquareValue, 1, 4);
            this.tableLayoutPanel1.Location = new System.Drawing.Point(12, 347);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 5;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 20F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 20F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(300, 100);
            this.tableLayoutPanel1.TabIndex = 3;
 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(3, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(85, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Выборочное среднее:";

            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(3, 20);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(85, 13);
            this.label2.TabIndex = 1;
            this.label2.Text = "Абсолютная погрешность среднего:";

            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(3, 40);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(85, 13);
            this.label3.TabIndex = 2;
            this.label3.Text = "Выборочная дисперсия:";

            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(3, 60);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(85, 13);
            this.label4.TabIndex = 3;
            this.label4.Text = "Относительная погрешность дисперсии:";

            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(3, 80);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(85, 13);
            this.label5.TabIndex = 4;
            this.label5.Text = "Хи-квадрат:";

            this.lblSampleMeanValue.AutoSize = true;
            this.lblSampleMeanValue.Location = new System.Drawing.Point(213, 0);
            this.lblSampleMeanValue.Name = "lblSampleMeanValue";
            this.lblSampleMeanValue.Size = new System.Drawing.Size(0, 13);
            this.lblSampleMeanValue.TabIndex = 5;

            this.lblRelErrorMeanValue.AutoSize = true;
            this.lblRelErrorMeanValue.Location = new System.Drawing.Point(213, 20);
            this.lblRelErrorMeanValue.Name = "lblRelErrorMeanValue";
            this.lblRelErrorMeanValue.Size = new System.Drawing.Size(0, 13);
            this.lblRelErrorMeanValue.TabIndex = 6;

            this.lblSampleVarianceValue.AutoSize = true;
            this.lblSampleVarianceValue.Location = new System.Drawing.Point(213, 40);
            this.lblSampleVarianceValue.Name = "lblSampleVarianceValue";
            this.lblSampleVarianceValue.Size = new System.Drawing.Size(0, 13);
            this.lblSampleVarianceValue.TabIndex = 7;

            this.lblRelErrorVarianceValue.AutoSize = true;
            this.lblRelErrorVarianceValue.Location = new System.Drawing.Point(213, 60);
            this.lblRelErrorVarianceValue.Name = "lblRelErrorVarianceValue";
            this.lblRelErrorVarianceValue.Size = new System.Drawing.Size(0, 13);
            this.lblRelErrorVarianceValue.TabIndex = 8;

            this.lblChiSquareValue.AutoSize = true;
            this.lblChiSquareValue.Location = new System.Drawing.Point(213, 80);
            this.lblChiSquareValue.Name = "lblChiSquareValue";
            this.lblChiSquareValue.Size = new System.Drawing.Size(0, 13);
            this.lblChiSquareValue.TabIndex = 9;

            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(624, 447);
            this.Controls.Add(this.tableLayoutPanel1);
            this.Controls.Add(this.chartHistogram);
            this.Controls.Add(this.btnGenerate);
            this.Controls.Add(this.cmbN);
            this.Name = "Form1";
            this.Text = "Моделирование нормальной случайной величины";
            ((System.ComponentModel.ISupportInitialize)(this.chartHistogram)).EndInit();
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();
        }

        private System.Windows.Forms.ComboBox cmbN;
        private System.Windows.Forms.Button btnGenerate;
        private System.Windows.Forms.DataVisualization.Charting.Chart chartHistogram;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label lblSampleMeanValue;
        private System.Windows.Forms.Label lblRelErrorMeanValue;
        private System.Windows.Forms.Label lblSampleVarianceValue;
        private System.Windows.Forms.Label lblRelErrorVarianceValue;
        private System.Windows.Forms.Label lblChiSquareValue;
    }
}