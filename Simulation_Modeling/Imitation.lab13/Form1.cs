using System;
using System.Drawing;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

public partial class Form1 : Form
{
    private System.Windows.Forms.Timer timer;
    private double euroPrice;
    private double dollarPrice;
    private double muEuro = 0.005;
    private double muDollar = 0.008;
    private double sigma = 0.1;
    private Random rand;
    private Series euroSeries;
    private Series dollarSeries;

    public Form1()
    {
        InitializeComponent();
        rand = new Random();
        timer = new System.Windows.Forms.Timer();
        timer.Interval = 100;
        timer.Tick += Timer_Tick;

        chartCurrency.Series.Clear();
        euroSeries = new Series("Euro") { ChartType = SeriesChartType.Line, Color = Color.Green };
        dollarSeries = new Series("Dollar") { ChartType = SeriesChartType.Line, Color = Color.Red };
        chartCurrency.Series.Add(euroSeries);
        chartCurrency.Series.Add(dollarSeries);
        chartCurrency.ChartAreas[0].AxisX.Title = "Time (s)";
        chartCurrency.ChartAreas[0].AxisY.Title = "Price";

        euroPrice = 91;
        dollarPrice = 80;
        txtEuroPrice.Text = euroPrice.ToString("F4");
        txtDollarPrice.Text = dollarPrice.ToString("F4");

        btnStartStop.Text = "Start";
    }

    private void btnStartStop_Click(object sender, EventArgs e)
    {
        if (timer.Enabled)
        {
            timer.Stop();
            btnStartStop.Text = "Start";
        }
        else
        {
            if (!double.TryParse(txtEuroPrice.Text, out euroPrice) || !double.TryParse(txtDollarPrice.Text, out dollarPrice))
            {
                MessageBox.Show("Пожалуйста, введите корректные начальные цены.");
                return;
            }
            euroSeries.Points.Clear();
            dollarSeries.Points.Clear();
            euroSeries.Points.AddXY(0, euroPrice);
            dollarSeries.Points.AddXY(0, dollarPrice);
            timer.Start();
            btnStartStop.Text = "Stop";
        }
    }

    private void Timer_Tick(object sender, EventArgs e)
    {
        double dt = 0.1; 
        double zetaEuro = rand.NextDouble() * 2 - 1;
        double zetaDollar = rand.NextDouble() * 2 - 1;

        euroPrice += muEuro * euroPrice * dt + sigma * euroPrice * Math.Sqrt(dt) * zetaEuro;
        dollarPrice += muDollar * dollarPrice * dt + sigma * dollarPrice * Math.Sqrt(dt) * zetaDollar;

        euroSeries.Points.AddXY(euroSeries.Points.Count * dt, euroPrice);
        dollarSeries.Points.AddXY(dollarSeries.Points.Count * dt, dollarPrice);

        txtEuroPrice.Text = euroPrice.ToString("F4");
        txtDollarPrice.Text = dollarPrice.ToString("F4");
    }
}