using System.Windows.Forms.DataVisualization.Charting;

namespace lab_2__exchange_rates_
{
    partial class Form1
    {
        /// <summary>
        /// Обязательная переменная конструктора.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Освободить все используемые ресурсы.
        /// </summary>
        /// <param name="disposing">истинно, если управляемый ресурс должен быть удален; иначе ложно.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Код, автоматически созданный конструктором форм Windows

        /// <summary>
        /// Требуемый метод для поддержки конструктора — не изменяйте 
        /// содержимое этого метода с помощью редактора кода.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.DataVisualization.Charting.ChartArea chartArea1 = new System.Windows.Forms.DataVisualization.Charting.ChartArea();
            System.Windows.Forms.DataVisualization.Charting.Legend legend1 = new System.Windows.Forms.DataVisualization.Charting.Legend();
            System.Windows.Forms.DataVisualization.Charting.Legend legend2 = new System.Windows.Forms.DataVisualization.Charting.Legend();
            System.Windows.Forms.DataVisualization.Charting.Series series1 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series2 = new System.Windows.Forms.DataVisualization.Charting.Series();
            this.chart1 = new System.Windows.Forms.DataVisualization.Charting.Chart();
            this.panel1 = new System.Windows.Forms.Panel();
            this.forecastRange = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.startButton = new System.Windows.Forms.Button();
            this.label = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.chart1)).BeginInit();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // chart1
            // 
            this.chart1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            this.chart1.BorderlineColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            chartArea1.AxisX.LabelStyle.Font = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisX.LabelStyle.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisX.LabelStyle.Format = "0";
            chartArea1.AxisX.LineColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisX.LineWidth = 3;
            chartArea1.AxisX.MajorGrid.LineColor = System.Drawing.SystemColors.ControlDarkDark;
            chartArea1.AxisX.Title = "Дни";
            chartArea1.AxisX.TitleFont = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisX.TitleForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisY.LabelStyle.Font = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisY.LabelStyle.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisY.LabelStyle.Format = "0.00";
            chartArea1.AxisY.LineColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisY.LineWidth = 3;
            chartArea1.AxisY.MajorGrid.LineColor = System.Drawing.SystemColors.ControlDarkDark;
            chartArea1.AxisY.Title = "Обменнный курс (Руб.)";
            chartArea1.AxisY.TitleFont = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisY.TitleForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            chartArea1.BorderColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.Name = "ChartArea1";
            this.chart1.ChartAreas.Add(chartArea1);
            legend1.Alignment = System.Drawing.StringAlignment.Center;
            legend1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            legend1.Docking = System.Windows.Forms.DataVisualization.Charting.Docking.Bottom;
            legend1.Font = new System.Drawing.Font("Century Gothic", 16F);
            legend1.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            legend1.IsTextAutoFit = false;
            legend1.Name = "Legend1";
            legend2.Alignment = System.Drawing.StringAlignment.Center;
            legend2.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            legend2.Docking = System.Windows.Forms.DataVisualization.Charting.Docking.Bottom;
            legend2.Font = new System.Drawing.Font("Century Gothic", 16F);
            legend2.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            legend2.IsTextAutoFit = false;
            legend2.Name = "Legend2";
            this.chart1.Legends.Add(legend1);
            this.chart1.Legends.Add(legend2);
            this.chart1.Location = new System.Drawing.Point(18, 18);
            this.chart1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.chart1.Name = "chart1";
            series1.BorderWidth = 2;
            series1.ChartArea = "ChartArea1";
            series1.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series1.Legend = "Legend1";
            series1.Name = "Доллар";
            series2.BorderWidth = 2;
            series2.ChartArea = "ChartArea1";
            series2.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series2.Legend = "Legend2";
            series2.Name = "Евро";
            this.chart1.Series.Add(series1);
            this.chart1.Series.Add(series2);
            this.chart1.Size = new System.Drawing.Size(1038, 718);
            this.chart1.TabIndex = 0;
            this.chart1.Text = "chart1";
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            this.panel1.Controls.Add(this.forecastRange);
            this.panel1.Controls.Add(this.label1);
            this.panel1.Controls.Add(this.startButton);
            this.panel1.Controls.Add(this.label);
            this.panel1.Location = new System.Drawing.Point(1096, 105);
            this.panel1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(553, 468);
            this.panel1.TabIndex = 1;
            // 
            // forecastRange
            // 
            this.forecastRange.Font = new System.Drawing.Font("Century Gothic", 36F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.forecastRange.Location = new System.Drawing.Point(320, 155);
            this.forecastRange.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.forecastRange.Name = "forecastRange";
            this.forecastRange.Size = new System.Drawing.Size(172, 96);
            this.forecastRange.TabIndex = 3;
            this.forecastRange.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Century Gothic", 20.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.label1.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.label1.Location = new System.Drawing.Point(78, 155);
            this.label1.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(237, 100);
            this.label1.TabIndex = 2;
            this.label1.Text = "Дальность\nпрогноза:";
            // 
            // startButton
            // 
            this.startButton.Font = new System.Drawing.Font("Century Gothic", 36F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.startButton.Location = new System.Drawing.Point(87, 305);
            this.startButton.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.startButton.Name = "startButton";
            this.startButton.Size = new System.Drawing.Size(406, 115);
            this.startButton.TabIndex = 1;
            this.startButton.Text = "Старт";
            this.startButton.UseVisualStyleBackColor = true;
            this.startButton.Click += new System.EventHandler(this.StartButton_Click);
            // 
            // label
            // 
            this.label.AutoSize = true;
            this.label.Font = new System.Drawing.Font("Century Gothic", 36F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.label.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.label.Location = new System.Drawing.Point(54, 17);
            this.label.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label.Name = "label";
            this.label.Size = new System.Drawing.Size(493, 87);
            this.label.TabIndex = 0;
            this.label.Text = "Курсы валют";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackgroundImage = global::lab_2__exchange_rates_.Properties.Resources.background;
            this.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.ClientSize = new System.Drawing.Size(1753, 803);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.chart1);
            this.DoubleBuffered = true;
            this.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.Name = "Form1";
            this.Text = "Form1";
            this.Load += new System.EventHandler(this.Form1_Load);
            ((System.ComponentModel.ISupportInitialize)(this.chart1)).EndInit();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.DataVisualization.Charting.Chart chart1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label;
        private System.Windows.Forms.Button startButton;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox forecastRange;
    }
}

