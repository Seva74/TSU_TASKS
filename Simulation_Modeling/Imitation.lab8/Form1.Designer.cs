namespace StochasticModeling
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
            this.txtQuestionLab1 = new System.Windows.Forms.TextBox();
            this.btnAnswer = new System.Windows.Forms.Button();
            this.lblAnswer = new System.Windows.Forms.Label();
            this.txtQuestionLab2 = new System.Windows.Forms.TextBox();
            this.btnPredict = new System.Windows.Forms.Button();
            this.lblPrediction = new System.Windows.Forms.Label();
            this.dgvEvents = new System.Windows.Forms.DataGridView();
            this.colEvent = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colProbability = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.txtN = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.btnSimulate = new System.Windows.Forms.Button();
            this.chartResults = new System.Windows.Forms.DataVisualization.Charting.Chart();
            ((System.ComponentModel.ISupportInitialize)(this.dgvEvents)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.chartResults)).BeginInit();
            this.SuspendLayout();

            this.txtQuestionLab1.Location = new System.Drawing.Point(12, 12);
            this.txtQuestionLab1.Multiline = true;
            this.txtQuestionLab1.Name = "txtQuestionLab1";
            this.txtQuestionLab1.Size = new System.Drawing.Size(300, 50);
            this.txtQuestionLab1.TabIndex = 0;
            this.txtQuestionLab1.Text = "Пойти сегодня в университет?";

            this.btnAnswer.Location = new System.Drawing.Point(12, 68);
            this.btnAnswer.Name = "btnAnswer";
            this.btnAnswer.Size = new System.Drawing.Size(300, 30);
            this.btnAnswer.TabIndex = 1;
            this.btnAnswer.Text = "Получить ответ (Лаба 8.1)";
            this.btnAnswer.UseVisualStyleBackColor = true;
            this.btnAnswer.Click += new System.EventHandler(this.btnAnswer_Click);

            this.lblAnswer.AutoSize = true;
            this.lblAnswer.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold);
            this.lblAnswer.Location = new System.Drawing.Point(12, 101);
            this.lblAnswer.Name = "lblAnswer";
            this.lblAnswer.Size = new System.Drawing.Size(0, 20);
            this.lblAnswer.TabIndex = 2;

            this.txtQuestionLab2.Location = new System.Drawing.Point(12, 131);
            this.txtQuestionLab2.Multiline = true;
            this.txtQuestionLab2.Name = "txtQuestionLab2";
            this.txtQuestionLab2.Size = new System.Drawing.Size(300, 50);
            this.txtQuestionLab2.TabIndex = 3;
            this.txtQuestionLab2.Text = "Пойти сегодня в университет?";

            this.btnPredict.Location = new System.Drawing.Point(12, 187);
            this.btnPredict.Name = "btnPredict";
            this.btnPredict.Size = new System.Drawing.Size(300, 30);
            this.btnPredict.TabIndex = 4;
            this.btnPredict.Text = "Получить предсказание (Лаба 8.2)";
            this.btnPredict.UseVisualStyleBackColor = true;
            this.btnPredict.Click += new System.EventHandler(this.btnPredict_Click);

            this.lblPrediction.AutoSize = true;
            this.lblPrediction.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold);
            this.lblPrediction.Location = new System.Drawing.Point(12, 220);
            this.lblPrediction.Name = "lblPrediction";
            this.lblPrediction.Size = new System.Drawing.Size(0, 20);
            this.lblPrediction.TabIndex = 5;

            this.dgvEvents.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgvEvents.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.colEvent,
            this.colProbability});
            this.dgvEvents.Location = new System.Drawing.Point(12, 250);
            this.dgvEvents.Name = "dgvEvents";
            this.dgvEvents.Size = new System.Drawing.Size(200, 100);
            this.dgvEvents.TabIndex = 6;

            this.colEvent.HeaderText = "Событие";
            this.colEvent.Name = "colEvent";

            this.colProbability.HeaderText = "Вероятность";
            this.colProbability.Name = "colProbability";

            this.txtN.Location = new System.Drawing.Point(12, 356);
            this.txtN.Name = "txtN";
            this.txtN.Size = new System.Drawing.Size(100, 20);
            this.txtN.TabIndex = 7;
            this.txtN.Text = "10000";

            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 340);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(15, 13);
            this.label1.TabIndex = 8;
            this.label1.Text = "N";

            this.btnSimulate.Location = new System.Drawing.Point(118, 354);
            this.btnSimulate.Name = "btnSimulate";
            this.btnSimulate.Size = new System.Drawing.Size(94, 23);
            this.btnSimulate.TabIndex = 9;
            this.btnSimulate.Text = "Симулировать (Лаба 8.3)";
            this.btnSimulate.UseVisualStyleBackColor = true;
            this.btnSimulate.Click += new System.EventHandler(this.btnSimulate_Click);

            chartArea1.Name = "ChartArea1";
            this.chartResults.ChartAreas.Add(chartArea1);
            legend1.Name = "Legend1";
            this.chartResults.Legends.Add(legend1);
            this.chartResults.Location = new System.Drawing.Point(12, 382);
            this.chartResults.Name = "chartResults";
            this.chartResults.Size = new System.Drawing.Size(600, 200);
            this.chartResults.TabIndex = 10;
            this.chartResults.Text = "chart1";

            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(624, 594);
            this.Controls.Add(this.chartResults);
            this.Controls.Add(this.btnSimulate);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.txtN);
            this.Controls.Add(this.dgvEvents);
            this.Controls.Add(this.lblPrediction);
            this.Controls.Add(this.btnPredict);
            this.Controls.Add(this.txtQuestionLab2);
            this.Controls.Add(this.lblAnswer);
            this.Controls.Add(this.btnAnswer);
            this.Controls.Add(this.txtQuestionLab1);
            this.Name = "Form1";
            this.Text = "Стохастическое моделирование: Лабы 8.1, 8.2, 8.3";
            ((System.ComponentModel.ISupportInitialize)(this.dgvEvents)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.chartResults)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();
        }

        private System.Windows.Forms.TextBox txtQuestionLab1;
        private System.Windows.Forms.Button btnAnswer;
        private System.Windows.Forms.Label lblAnswer;
        private System.Windows.Forms.TextBox txtQuestionLab2;
        private System.Windows.Forms.Button btnPredict;
        private System.Windows.Forms.Label lblPrediction;
        private System.Windows.Forms.DataGridView dgvEvents;
        private System.Windows.Forms.DataGridViewTextBoxColumn colEvent;
        private System.Windows.Forms.DataGridViewTextBoxColumn colProbability;
        private System.Windows.Forms.TextBox txtN;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button btnSimulate;
        private System.Windows.Forms.DataVisualization.Charting.Chart chartResults;
    }
}