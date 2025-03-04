using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace lab_2__exchange_rates_
{
    public partial class Form1 : Form
    {
        private ChartArea chartArea;
        private Timer timer = new Timer();
        private int currentIndex = 0;
        const double k = 0.02;
        private double rate_USD = 90.65;
        private double rate_EURO = 99.69;
        private int range;
        double max_USD = 0;
        double maxY_EURO = 0;
        List<double> stat_EURO = new List<double>();
        List<double> stat_USD = new List<double>();
        public Form1()
        {
            InitializeComponent();
            timer.Tick += Timer_Tick;
        }

        private void StartButton_Click(object sender, EventArgs e)
        {
            int.TryParse(forecastRange.Text, out int deltaRange);
            range += deltaRange;
            Random rnd = new Random();
            for (int i = 0; i < deltaRange; i++)
            {
                stat_USD.Add(rate_USD);
                rate_USD = rate_USD * (1 + k * (rnd.NextDouble() - 0.5));
                stat_EURO.Add(rate_EURO);
                rate_EURO = rate_EURO * (1 + k * (rnd.NextDouble() - 0.5));
            }

            // Настройка графика
            chartArea = chart1.ChartAreas[0];

            // Установка минимального значения по оси X
            chartArea.AxisX.Minimum = 0;
            // Установка максимального значения по оси X
            chartArea.AxisX.Maximum = range;
            // Установка размера шага по оси X
            chartArea.AxisX.Interval = (range < 50) ? 1.0 : range / 50.0; ;

            // Установка минимального значения по оси Y
            chartArea.AxisY.Minimum = 0;
            // Установка максимального значения по оси Y
            chartArea.AxisY.Maximum = Math.Max(stat_EURO.Max(), stat_USD.Max());
            // Установка размера шага по оси Y
            chartArea.AxisY.Interval = chartArea.AxisY.Maximum / 10;

            // Инициализация таймера
            timer.Interval = 5000 / deltaRange;
            timer.Start();

        }

        private void Timer_Tick(object sender, EventArgs e)
        {
            // Обработчик события таймера

            // Проверка на выход за пределы списка
            if (currentIndex < range)
            {

                // Ваш код обработки текущей точки, например, вывод в консоль
                chart1.Series[0].Points.AddXY(currentIndex, stat_USD[currentIndex]);
                chart1.Series[1].Points.AddXY(currentIndex, stat_EURO[currentIndex]);

                // Увеличение индекса для следующей итерации
                currentIndex++;
            }
            else
            {
                // Если все точки обработаны, остановите таймер
                timer.Stop();
            }
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }
    }
}
