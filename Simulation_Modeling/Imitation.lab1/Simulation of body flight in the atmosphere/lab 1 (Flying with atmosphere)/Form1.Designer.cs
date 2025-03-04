using System.Drawing;
using System.Windows.Forms.DataVisualization.Charting;

namespace lab_1__Flying_with_atmosphere_
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
            System.Windows.Forms.DataVisualization.Charting.Legend legend3 = new System.Windows.Forms.DataVisualization.Charting.Legend();
            System.Windows.Forms.DataVisualization.Charting.Series series1 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series2 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series3 = new System.Windows.Forms.DataVisualization.Charting.Series();
            this.chart1 = new System.Windows.Forms.DataVisualization.Charting.Chart();
            this.panel1 = new System.Windows.Forms.Panel();
            this.label1 = new System.Windows.Forms.Label();
            this.panel2 = new System.Windows.Forms.Panel();
            this.ResultsDataGrid = new System.Windows.Forms.DataGridView();
            this.EndSpeed = new System.Windows.Forms.Button();
            this.Distance = new System.Windows.Forms.Button();
            this.button1 = new System.Windows.Forms.Button();
            this.MaxYOutput = new System.Windows.Forms.Button();
            this.inputFrequency = new System.Windows.Forms.TextBox();
            this.desk = new System.Windows.Forms.Label();
            this.inputSize = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.inputWeight = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.inputAngle = new System.Windows.Forms.TextBox();
            this.inputSpeed = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.chart1)).BeginInit();
            this.panel1.SuspendLayout();
            this.panel2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.ResultsDataGrid)).BeginInit();
            this.SuspendLayout();
            // 
            // chart1
            // 
            this.chart1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            this.chart1.BorderlineColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            chartArea1.AxisX.LabelStyle.Font = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisX.LabelStyle.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisX.LabelStyle.Format = "0.0";
            chartArea1.AxisX.LineColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisX.LineWidth = 3;
            chartArea1.AxisX.MajorGrid.LineColor = System.Drawing.SystemColors.ControlDarkDark;
            chartArea1.AxisX.Title = "Расстояние (м)";
            chartArea1.AxisX.TitleFont = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisX.TitleForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisY.LabelStyle.Font = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisY.LabelStyle.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisY.LabelStyle.Format = "0.0";
            chartArea1.AxisY.LineColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.AxisY.LineWidth = 3;
            chartArea1.AxisY.MajorGrid.LineColor = System.Drawing.SystemColors.ControlDarkDark;
            chartArea1.AxisY.Title = "Высота (м)";
            chartArea1.AxisY.TitleFont = new System.Drawing.Font("Century Gothic", 12F, System.Drawing.FontStyle.Bold);
            chartArea1.AxisY.TitleForeColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            chartArea1.BorderColor = System.Drawing.SystemColors.AppWorkspace;
            chartArea1.Name = "ChartArea1";
            this.chart1.ChartAreas.Add(chartArea1);
            legend1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            legend1.Font = new System.Drawing.Font("Century Gothic", 16F);
            legend1.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            legend1.IsTextAutoFit = false;
            legend1.Name = "Legend1";
            legend1.Position.Auto = false;
            legend1.Position.Height = 12F;
            legend1.Position.Width = 30F;
            legend1.Position.X = 10F;
            legend1.Position.Y = 88F;
            legend2.Alignment = System.Drawing.StringAlignment.Center;
            legend2.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            legend2.Docking = System.Windows.Forms.DataVisualization.Charting.Docking.Bottom;
            legend2.Font = new System.Drawing.Font("Century Gothic", 16F);
            legend2.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            legend2.IsTextAutoFit = false;
            legend2.Name = "Legend2";
            legend3.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            legend3.Font = new System.Drawing.Font("Century Gothic", 16F);
            legend3.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            legend3.IsTextAutoFit = false;
            legend3.Name = "Legend3";
            legend3.Position.Auto = false;
            legend3.Position.Height = 12F;
            legend3.Position.Width = 30F;
            legend3.Position.X = 65F;
            legend3.Position.Y = 88F;
            this.chart1.Legends.Add(legend1);
            this.chart1.Legends.Add(legend2);
            this.chart1.Legends.Add(legend3);
            this.chart1.Location = new System.Drawing.Point(-96, 65);
            this.chart1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.chart1.Name = "chart1";
            this.chart1.Palette = System.Windows.Forms.DataVisualization.Charting.ChartColorPalette.None;
            series1.BorderWidth = 2;
            series1.ChartArea = "ChartArea1";
            series1.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series1.Legend = "Legend1";
            series1.Name = "График 1";
            series2.BorderWidth = 2;
            series2.ChartArea = "ChartArea1";
            series2.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series2.Legend = "Legend2";
            series2.Name = "График 2";
            series3.BorderWidth = 2;
            series3.ChartArea = "ChartArea1";
            series3.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series3.Legend = "Legend3";
            series3.Name = "График 3";
            this.chart1.Series.Add(series1);
            this.chart1.Series.Add(series2);
            this.chart1.Series.Add(series3);
            this.chart1.Size = new System.Drawing.Size(1144, 730);
            this.chart1.TabIndex = 0;
            this.chart1.Text = "chart1";
            this.chart1.Click += new System.EventHandler(this.chart1_Click);
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            this.panel1.Controls.Add(this.label1);
            this.panel1.Location = new System.Drawing.Point(1068, 14);
            this.panel1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(678, 94);
            this.panel1.TabIndex = 1;
            this.panel1.Paint += new System.Windows.Forms.PaintEventHandler(this.panel1_Paint);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Century Gothic", 36F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.label1.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.label1.Location = new System.Drawing.Point(105, 0);
            this.label1.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(468, 87);
            this.label1.TabIndex = 2;
            this.label1.Text = "Параметры";
            // 
            // panel2
            // 
            this.panel2.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(25)))), ((int)(((byte)(23)))), ((int)(((byte)(24)))));
            this.panel2.Controls.Add(this.ResultsDataGrid);
            this.panel2.Controls.Add(this.EndSpeed);
            this.panel2.Controls.Add(this.Distance);
            this.panel2.Controls.Add(this.button1);
            this.panel2.Controls.Add(this.MaxYOutput);
            this.panel2.Controls.Add(this.inputFrequency);
            this.panel2.Controls.Add(this.desk);
            this.panel2.Controls.Add(this.inputSize);
            this.panel2.Controls.Add(this.label6);
            this.panel2.Controls.Add(this.inputWeight);
            this.panel2.Controls.Add(this.label4);
            this.panel2.Controls.Add(this.inputAngle);
            this.panel2.Controls.Add(this.inputSpeed);
            this.panel2.Controls.Add(this.label3);
            this.panel2.Controls.Add(this.label2);
            this.panel2.Location = new System.Drawing.Point(1068, 132);
            this.panel2.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(678, 740);
            this.panel2.TabIndex = 3;
            this.panel2.Paint += new System.Windows.Forms.PaintEventHandler(this.panel2_Paint);
            // 
            // ResultsDataGrid
            // 
            this.ResultsDataGrid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.ResultsDataGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.ResultsDataGrid.Location = new System.Drawing.Point(59, 559);
            this.ResultsDataGrid.Name = "ResultsDataGrid";
            this.ResultsDataGrid.RowHeadersWidth = 62;
            this.ResultsDataGrid.RowTemplate.Height = 28;
            this.ResultsDataGrid.Size = new System.Drawing.Size(564, 168);
            this.ResultsDataGrid.TabIndex = 17;
            // 
            // EndSpeed
            // 
            this.EndSpeed.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.EndSpeed.Location = new System.Drawing.Point(469, 404);
            this.EndSpeed.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.EndSpeed.Name = "EndSpeed";
            this.EndSpeed.Size = new System.Drawing.Size(195, 69);
            this.EndSpeed.TabIndex = 16;
            this.EndSpeed.Text = "ESpeed";
            this.EndSpeed.UseVisualStyleBackColor = true;
            // 
            // Distance
            // 
            this.Distance.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.Distance.Location = new System.Drawing.Point(22, 404);
            this.Distance.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.Distance.Name = "Distance";
            this.Distance.Size = new System.Drawing.Size(207, 69);
            this.Distance.TabIndex = 15;
            this.Distance.Text = "Distance";
            this.Distance.UseVisualStyleBackColor = true;
            // 
            // button1
            // 
            this.button1.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.button1.Location = new System.Drawing.Point(257, 482);
            this.button1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(180, 69);
            this.button1.TabIndex = 7;
            this.button1.Text = "Запуск";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.Button1_Click);
            // 
            // MaxYOutput
            // 
            this.MaxYOutput.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.MaxYOutput.Location = new System.Drawing.Point(257, 403);
            this.MaxYOutput.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.MaxYOutput.Name = "MaxYOutput";
            this.MaxYOutput.Size = new System.Drawing.Size(180, 69);
            this.MaxYOutput.TabIndex = 14;
            this.MaxYOutput.Text = "MaxY";
            this.MaxYOutput.UseVisualStyleBackColor = true;
            // 
            // inputFrequency
            // 
            this.inputFrequency.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.inputFrequency.Location = new System.Drawing.Point(355, 311);
            this.inputFrequency.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.inputFrequency.Name = "inputFrequency";
            this.inputFrequency.Size = new System.Drawing.Size(157, 57);
            this.inputFrequency.TabIndex = 13;
            this.inputFrequency.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // desk
            // 
            this.desk.AutoSize = true;
            this.desk.Font = new System.Drawing.Font("Century Gothic", 20.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.desk.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.desk.Location = new System.Drawing.Point(13, 311);
            this.desk.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.desk.Name = "desk";
            this.desk.Size = new System.Drawing.Size(197, 50);
            this.desk.TabIndex = 12;
            this.desk.Text = "Частота:";
            // 
            // inputSize
            // 
            this.inputSize.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.inputSize.Location = new System.Drawing.Point(355, 234);
            this.inputSize.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.inputSize.Name = "inputSize";
            this.inputSize.Size = new System.Drawing.Size(157, 57);
            this.inputSize.TabIndex = 11;
            this.inputSize.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Century Gothic", 20.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.label6.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.label6.Location = new System.Drawing.Point(13, 234);
            this.label6.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(298, 50);
            this.label6.TabIndex = 10;
            this.label6.Text = "Размер тела:";
            // 
            // inputWeight
            // 
            this.inputWeight.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.inputWeight.Location = new System.Drawing.Point(355, 158);
            this.inputWeight.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.inputWeight.Name = "inputWeight";
            this.inputWeight.Size = new System.Drawing.Size(157, 57);
            this.inputWeight.TabIndex = 9;
            this.inputWeight.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Century Gothic", 20.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.label4.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.label4.Location = new System.Drawing.Point(13, 161);
            this.label4.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(216, 50);
            this.label4.TabIndex = 8;
            this.label4.Text = "Вес тела:";
            // 
            // inputAngle
            // 
            this.inputAngle.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.inputAngle.Location = new System.Drawing.Point(355, 84);
            this.inputAngle.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.inputAngle.Name = "inputAngle";
            this.inputAngle.Size = new System.Drawing.Size(157, 57);
            this.inputAngle.TabIndex = 6;
            this.inputAngle.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // inputSpeed
            // 
            this.inputSpeed.Font = new System.Drawing.Font("Century Gothic", 20.25F);
            this.inputSpeed.Location = new System.Drawing.Point(355, 9);
            this.inputSpeed.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.inputSpeed.Name = "inputSpeed";
            this.inputSpeed.Size = new System.Drawing.Size(157, 57);
            this.inputSpeed.TabIndex = 5;
            this.inputSpeed.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Century Gothic", 20.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.label3.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.label3.Location = new System.Drawing.Point(13, 86);
            this.label3.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(293, 50);
            this.label3.TabIndex = 4;
            this.label3.Text = "Угол броска:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Century Gothic", 20.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.label2.ForeColor = System.Drawing.SystemColors.AppWorkspace;
            this.label2.Location = new System.Drawing.Point(13, 11);
            this.label2.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(311, 50);
            this.label2.TabIndex = 3;
            this.label2.Text = "Сила броска:";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.BackgroundImage = global::lab_1__Flying_with_atmosphere_.Properties.Resources.background;
            this.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.ClientSize = new System.Drawing.Size(1801, 886);
            this.Controls.Add(this.panel2);
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
            this.panel2.ResumeLayout(false);
            this.panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.ResultsDataGrid)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.DataVisualization.Charting.Chart chart1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.TextBox inputAngle;
        private System.Windows.Forms.TextBox inputSpeed;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.TextBox inputWeight;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox inputSize;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox inputFrequency;
        private System.Windows.Forms.Label desk;
        private System.Windows.Forms.Button MaxYOutput;
        private System.Windows.Forms.Button Distance;
        private System.Windows.Forms.Button EndSpeed;
        private System.Windows.Forms.DataGridView ResultsDataGrid;
    }
}

