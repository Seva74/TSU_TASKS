using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace lab_1__Flying_with_atmosphere_
{
    public partial class Form1 : Form
    {
        private ChartArea chartArea;
        private List<Series> seriesList = new List<Series>();
        private Timer timer = new Timer();
        private double angle;
        private double speed;
        private double size;
        private double frequency;
        private double weight;
        private int i = 0;
        double area = 0;
        const double g = 9.81;
        const double C = 0.15;
        const double rho = 1.29;
        double t, x, y, v0, cosa, sina, S, m, k, vx, vy;

        private int currentIndex = 0;

        private void chart1_Click(object sender, EventArgs e)
        {

        }

        private void label2_Click(object sender, EventArgs e)
        {

        }

        private List<Point> points = new List<Point>();
        private List<(double timeStep, double distance, double maxHeight, double endSpeed)> results = new List<(double, double, double, double)>();

        public class Point
        {
            public double X { get; }
            public double Y { get; }

            public Point(double x, double y)
            {
                X = x;
                Y = y;
            }
        }

        private void panel2_Paint(object sender, PaintEventArgs e)
        {
        }

        private void panel1_Paint(object sender, PaintEventArgs e)
        {
        }

        private void Form1_Load(object sender, EventArgs e)
        {
        }

        private void Button1_Click(object sender, EventArgs e)
        {
            if (!double.TryParse(inputSpeed.Text, out speed) || !double.TryParse(inputAngle.Text, out angle) ||
                !double.TryParse(inputSize.Text, out size) || !double.TryParse(inputWeight.Text, out weight) ||
                !double.TryParse(inputFrequency.Text, out frequency) || frequency <= 0)
            {
                MessageBox.Show("Пожалуйста, введите корректные положительные числовые значения.");
                return;
            }

            if (!timer.Enabled)
            {
                t = 0;
                x = 0;
                y = 0;
                double maxX = 0;
                double maxY = 0;
                v0 = speed;
                double a = angle * Math.PI / 180;
                cosa = Math.Cos(a);
                sina = Math.Sin(a);
                S = size;
                m = weight;
                k = 0.5 * C * rho * S / m;
                vx = v0 * cosa;
                vy = v0 * sina;
                points.Clear();
                points.Add(new Point(x, y));

                double lastVx = 0, lastVy = 0;
                do
                {
                    t += frequency;
                    double v = Math.Sqrt(vx * vx + vy * vy);
                    lastVx = vx - k * vx * v * frequency;
                    lastVy = vy - (g + k * vy * v) * frequency;
                    vx = lastVx;
                    vy = lastVy;
                    x = x + vx * frequency;
                    y = y + vy * frequency;
                    if (x > maxX) { maxX = x; }
                    if (y > maxY) { maxY = y; }
                    points.Add(new Point(x, y));
                } while (y > 0);

                area = Math.Max(area, Math.Max(maxX, maxY));
                chartArea = chart1.ChartAreas[0];
                chartArea.AxisX.Minimum = 0;
                chartArea.AxisX.Maximum = area;
                chartArea.AxisY.Minimum = 0;
                chartArea.AxisY.Maximum = area;
                chartArea.AxisX.Interval = area / 20;
                chartArea.AxisY.Interval = area / 20;

                double endSpeed = Math.Sqrt(lastVx * lastVx + lastVy * lastVy);
                MaxYOutput.Text = maxY.ToString("F2");
                Distance.Text = maxX.ToString("F2");
                EndSpeed.Text = endSpeed.ToString("F2");

                results.Add((frequency, maxX, maxY, endSpeed));
                UpdateResultsTable();

                if (i >= 3)
                {
                    i = 0;
                    chart1.Series[i].Points.Clear();
                }
                chart1.Series[i].Points.AddXY(x, y);

                timer.Interval = 25;
                timer.Start();
            }
        }

        private void Timer_Tick(object sender, EventArgs e)
        {
            if (currentIndex < points.Count)
            {
                Point currentPoint = points[currentIndex];
                chart1.Series[i].Points.AddXY(currentPoint.X, currentPoint.Y);
                currentIndex++;
            }
            else
            {
                timer.Stop();
                i++;
                if (i >= 3) i = 0;
                points.Clear();
                currentIndex = 0;
            }
        }

        private void UpdateResultsTable()
        {
            ResultsDataGrid.DataSource = null;
            ResultsDataGrid.DataSource = results.Select(r => new
            {
                Шаг_времени = r.timeStep.ToString("F2"),
                Дальность = r.distance.ToString("F2"),
                Макс_высота = r.maxHeight.ToString("F2"),
                Конечная_скорость = r.endSpeed.ToString("F2")
            }).ToList();
        }

        public Form1()
        {
            InitializeComponent();

            seriesList.Add(chart1.Series[0]);
            seriesList.Add(chart1.Series[1]);
            seriesList.Add(chart1.Series[2]);
            timer.Tick += Timer_Tick;
        }
    }
}