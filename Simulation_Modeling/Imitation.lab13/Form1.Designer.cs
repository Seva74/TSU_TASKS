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
        this.components = new System.ComponentModel.Container();
        System.Windows.Forms.DataVisualization.Charting.ChartArea chartArea1 = new System.Windows.Forms.DataVisualization.Charting.ChartArea();
        System.Windows.Forms.DataVisualization.Charting.Legend legend1 = new System.Windows.Forms.DataVisualization.Charting.Legend();
        this.chartCurrency = new System.Windows.Forms.DataVisualization.Charting.Chart();
        this.txtEuroPrice = new System.Windows.Forms.TextBox();
        this.txtDollarPrice = new System.Windows.Forms.TextBox();
        this.label1 = new System.Windows.Forms.Label();
        this.label2 = new System.Windows.Forms.Label();
        this.btnStartStop = new System.Windows.Forms.Button();
        ((System.ComponentModel.ISupportInitialize)(this.chartCurrency)).BeginInit();
        this.SuspendLayout();
        // 
        // chartCurrency
        // 
        chartArea1.Name = "ChartArea1";
        this.chartCurrency.ChartAreas.Add(chartArea1);
        legend1.Name = "Legend1";
        this.chartCurrency.Legends.Add(legend1);
        this.chartCurrency.Location = new System.Drawing.Point(12, 12);
        this.chartCurrency.Name = "chartCurrency";
        this.chartCurrency.Size = new System.Drawing.Size(400, 300);
        this.chartCurrency.TabIndex = 0;
        this.chartCurrency.Text = "Currency Rates";
        // 
        // txtEuroPrice
        // 
        this.txtEuroPrice.Location = new System.Drawing.Point(150, 320);
        this.txtEuroPrice.Name = "txtEuroPrice";
        this.txtEuroPrice.Size = new System.Drawing.Size(100, 20);
        this.txtEuroPrice.TabIndex = 1;
        // 
        // txtDollarPrice
        // 
        this.txtDollarPrice.Location = new System.Drawing.Point(150, 350);
        this.txtDollarPrice.Name = "txtDollarPrice";
        this.txtDollarPrice.Size = new System.Drawing.Size(100, 20);
        this.txtDollarPrice.TabIndex = 2;
        // 
        // label1
        // 
        this.label1.AutoSize = true;
        this.label1.Location = new System.Drawing.Point(50, 323);
        this.label1.Name = "label1";
        this.label1.Size = new System.Drawing.Size(94, 13);
        this.label1.TabIndex = 3;
        this.label1.Text = "Initial price (Euro):";
        // 
        // label2
        // 
        this.label2.AutoSize = true;
        this.label2.Location = new System.Drawing.Point(50, 353);
        this.label2.Name = "label2";
        this.label2.Size = new System.Drawing.Size(98, 13);
        this.label2.TabIndex = 4;
        this.label2.Text = "Initial price (Dollar):";
        // 
        // btnStartStop
        // 
        this.btnStartStop.Location = new System.Drawing.Point(150, 380);
        this.btnStartStop.Name = "btnStartStop";
        this.btnStartStop.Size = new System.Drawing.Size(100, 23);
        this.btnStartStop.TabIndex = 5;
        this.btnStartStop.Text = "Start";
        this.btnStartStop.UseVisualStyleBackColor = true;
        this.btnStartStop.Click += new System.EventHandler(this.btnStartStop_Click);
        // 
        // Form1
        // 
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.ClientSize = new System.Drawing.Size(424, 411);
        this.Controls.Add(this.btnStartStop);
        this.Controls.Add(this.label2);
        this.Controls.Add(this.label1);
        this.Controls.Add(this.txtDollarPrice);
        this.Controls.Add(this.txtEuroPrice);
        this.Controls.Add(this.chartCurrency);
        this.Name = "Form1";
        this.Text = "Currency Exchange Simulation";
        ((System.ComponentModel.ISupportInitialize)(this.chartCurrency)).EndInit();
        this.ResumeLayout(false);
        this.PerformLayout();
    }

    private System.Windows.Forms.DataVisualization.Charting.Chart chartCurrency;
    private System.Windows.Forms.TextBox txtEuroPrice;
    private System.Windows.Forms.TextBox txtDollarPrice;
    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.Label label2;
    private System.Windows.Forms.Button btnStartStop;
}