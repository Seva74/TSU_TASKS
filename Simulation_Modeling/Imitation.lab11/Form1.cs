using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace NormalRVSimulation
{
    public partial class Form1 : Form
    {
        private Random rand = new Random();

        public Form1()
        {
            InitializeComponent();
            cmbN.Items.AddRange(new string[] { "10", "100", "1000", "10000" });
            cmbN.SelectedIndex = 0;
        }

        private void btnGenerate_Click(object sender, EventArgs e)
        {
            int N = int.Parse(cmbN.SelectedItem.ToString());
            List<double> samples = new List<double>();
            for (int i = 0; i < N; i++)
            {
                samples.Add(GenerateStandardNormal());
            }

            double sum = 0;
            foreach (double x in samples)
                sum += x;
            double sampleMean = sum / N;

            double sumSq = 0;
            foreach (double x in samples)
                sumSq += Math.Pow(x - sampleMean, 2);
            double sampleVariance = sumSq / (N - 1);

            double absErrorMean = Math.Abs(sampleMean);
            double relErrorVariance = Math.Abs(sampleVariance - 1) / 1; 

            const double min = -4;
            const double max = 4;
            const int numBins = 16;
            double binWidth = (max - min) / numBins;
            double[] binEdges = new double[numBins + 1];
            for (int i = 0; i <= numBins; i++)
                binEdges[i] = min + i * binWidth;

            int[] frequencies = new int[numBins];
            foreach (double x in samples)
            {
                if (x >= min && x < max)
                {
                    int bin = (int)Math.Floor((x - min) / binWidth);
                    if (bin >= 0 && bin < numBins)
                        frequencies[bin]++;
                }
            }

            double[] expected = new double[numBins];
            for (int i = 0; i < numBins; i++)
            {
                double a = binEdges[i];
                double b = binEdges[i + 1];
                double p = NormalCDF(b) - NormalCDF(a);
                expected[i] = N * p;
            }

            double chiSquare = 0;
            for (int i = 0; i < numBins; i++)
            {
                if (expected[i] > 0)
                    chiSquare += Math.Pow(frequencies[i] - expected[i], 2) / expected[i];
            }

            chartHistogram.Series.Clear();
            Series series = new Series("Гистограмма");
            series.ChartType = SeriesChartType.Column;
            double maxFrequency = 0;
            for (int i = 0; i < numBins; i++)
            {
                series.Points.AddXY(binEdges[i] + binWidth / 2, frequencies[i]);
                if (frequencies[i] > maxFrequency)
                    maxFrequency = frequencies[i];
            }
            chartHistogram.Series.Add(series);
            chartHistogram.ChartAreas[0].AxisX.Title = "Значение";
            chartHistogram.ChartAreas[0].AxisY.Title = "Частота";
            chartHistogram.ChartAreas[0].AxisY.Maximum = maxFrequency * 1.1; // 10% запас
            chartHistogram.ChartAreas[0].AxisY.Minimum = 0;

            lblSampleMeanValue.Text = sampleMean.ToString("F4");
            lblRelErrorMeanValue.Text = absErrorMean.ToString("F4");
            lblSampleVarianceValue.Text = sampleVariance.ToString("F4");
            lblRelErrorVarianceValue.Text = relErrorVariance.ToString("F4");
            lblChiSquareValue.Text = chiSquare.ToString("F4");
        }

        private double GenerateStandardNormal()
        {
            double U1 = rand.NextDouble();
            double U2 = rand.NextDouble();
            return Math.Sqrt(-2 * Math.Log(U1)) * Math.Cos(2 * Math.PI * U2);
        }

        private double NormalCDF(double x)
        {
            const double p = 0.2316419;
            const double b1 = 0.31938153;
            const double b2 = -0.356563782;
            const double b3 = 1.781477937;
            const double b4 = -1.821255978;
            const double b5 = 1.330274429;
            double t = 1 / (1 + p * Math.Abs(x));
            double t2 = t * t;
            double t3 = t2 * t;
            double t4 = t3 * t;
            double t5 = t4 * t;
            double pdf = (1 / Math.Sqrt(2 * Math.PI)) * Math.Exp(-0.5 * x * x);
            double z = pdf * (b1 * t + b2 * t2 + b3 * t3 + b4 * t4 + b5 * t5);
            return x >= 0 ? 1 - z : z;
        }
    }
}