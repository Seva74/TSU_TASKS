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
        this.panelWeather = new System.Windows.Forms.Panel();
        this.lblWeather = new System.Windows.Forms.Label();
        this.lblTime = new System.Windows.Forms.Label();
        this.lblP1 = new System.Windows.Forms.Label();
        this.lblP2 = new System.Windows.Forms.Label();
        this.lblP3 = new System.Windows.Forms.Label();
        this.btnControl = new System.Windows.Forms.Button();
        this.panelWeather.SuspendLayout();
        this.SuspendLayout();
        // 
        // panelWeather
        // 
        this.panelWeather.Controls.Add(this.lblWeather);
        this.panelWeather.Controls.Add(this.lblTime);
        this.panelWeather.Controls.Add(this.lblP1);
        this.panelWeather.Controls.Add(this.lblP2);
        this.panelWeather.Controls.Add(this.lblP3);
        this.panelWeather.Location = new System.Drawing.Point(12, 12);
        this.panelWeather.Name = "panelWeather";
        this.panelWeather.Size = new System.Drawing.Size(260, 200);
        this.panelWeather.TabIndex = 0;
        // 
        // lblWeather
        // 
        this.lblWeather.AutoSize = true;
        this.lblWeather.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F);
        this.lblWeather.Location = new System.Drawing.Point(10, 10);
        this.lblWeather.Name = "lblWeather";
        this.lblWeather.Size = new System.Drawing.Size(51, 20);
        this.lblWeather.TabIndex = 0;
        this.lblWeather.Text = "Ясно";
        // 
        // lblTime
        // 
        this.lblTime.AutoSize = true;
        this.lblTime.Location = new System.Drawing.Point(10, 40);
        this.lblTime.Name = "lblTime";
        this.lblTime.Size = new System.Drawing.Size(60, 13);
        this.lblTime.TabIndex = 1;
        this.lblTime.Text = "0 дней";
        // 
        // lblP1
        // 
        this.lblP1.AutoSize = true;
        this.lblP1.Location = new System.Drawing.Point(10, 70);
        this.lblP1.Name = "lblP1";
        this.lblP1.Size = new System.Drawing.Size(100, 13);
        this.lblP1.TabIndex = 2;
        this.lblP1.Text = "P(Ясно): 0.0000";
        // 
        // lblP2
        // 
        this.lblP2.AutoSize = true;
        this.lblP2.Location = new System.Drawing.Point(10, 100);
        this.lblP2.Name = "lblP2";
        this.lblP2.Size = new System.Drawing.Size(100, 13);
        this.lblP2.TabIndex = 3;
        this.lblP2.Text = "P(Облачно): 0.0000";
        // 
        // lblP3
        // 
        this.lblP3.AutoSize = true;
        this.lblP3.Location = new System.Drawing.Point(10, 130);
        this.lblP3.Name = "lblP3";
        this.lblP3.Size = new System.Drawing.Size(100, 13);
        this.lblP3.TabIndex = 4;
        this.lblP3.Text = "P(Пасмурно): 0.0000";
        // 
        // btnControl
        // 
        this.btnControl.Location = new System.Drawing.Point(12, 218);
        this.btnControl.Name = "btnControl";
        this.btnControl.Size = new System.Drawing.Size(100, 23);
        this.btnControl.TabIndex = 5;
        this.btnControl.Text = "Старт";
        this.btnControl.UseVisualStyleBackColor = true;
        this.btnControl.Click += new System.EventHandler(this.btnControl_Click);
        // 
        // Form1
        // 
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.ClientSize = new System.Drawing.Size(284, 261);
        this.Controls.Add(this.btnControl);
        this.Controls.Add(this.panelWeather);
        this.Name = "Form1";
        this.Text = "Симуляция погоды";
        this.panelWeather.ResumeLayout(false);
        this.panelWeather.PerformLayout();
        this.ResumeLayout(false);
    }

    private System.Windows.Forms.Panel panelWeather;
    private System.Windows.Forms.Label lblWeather;
    private System.Windows.Forms.Label lblTime;
    private System.Windows.Forms.Label lblP1;
    private System.Windows.Forms.Label lblP2;
    private System.Windows.Forms.Label lblP3;
    private System.Windows.Forms.Button btnControl;
}