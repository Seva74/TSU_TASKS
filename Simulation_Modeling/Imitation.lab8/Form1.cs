using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace StochasticModeling
{
    public partial class Form1 : Form
    {
        private string[] magic8BallAnswers = {
            "��� ��������", "����� ���� ���������", "���������� ��", "��������� �����", "������� �����������",
            "�� ������������ ������", "��", "������ �� ��", "����� ������� � ��", "�� � ����������",
            "������ �����", "������ ������ �����������", "��������������� � ������ �����", "���� �� �����",
            "��� ����� � ���", "�� ���� ������ � ���", "������ �����������"
        };

        public Form1()
        {
            InitializeComponent();
            dgvEvents.Rows.Add("������� 1", 0.2);
            dgvEvents.Rows.Add("������� 2", 0.3);
            dgvEvents.Rows.Add("������� 3", 0.5);
        }

        private void btnAnswer_Click(object sender, EventArgs e)
        {
            Random rand = new Random();
            lblAnswer.Text = rand.Next(2) == 0 ? "��" : "���";
        }

        private void btnPredict_Click(object sender, EventArgs e)
        {
            Random rand = new Random();
            lblPrediction.Text = magic8BallAnswers[rand.Next(magic8BallAnswers.Length)];
        }
        private void btnSimulate_Click(object sender, EventArgs e)
        {
            List<string> events = new List<string>();
            List<double> probabilities = new List<double>();
            foreach (DataGridViewRow row in dgvEvents.Rows)
            {
                if (row.Cells[0].Value != null && row.Cells[1].Value != null)
                {
                    try
                    {
                        events.Add(row.Cells[0].Value.ToString());
                        probabilities.Add(double.Parse(row.Cells[1].Value.ToString()));
                    }
                    catch
                    {
                        MessageBox.Show("������� ���������� ����������� (�����).");
                        return;
                    }
                }
            }

            double sum = 0;
            foreach (double p in probabilities)
                sum += p;
            if (Math.Abs(sum - 1) > 1e-6)
            {
                MessageBox.Show("����� ������������ ������ ���� ����� 1.");
                return;
            }

            if (!int.TryParse(txtN.Text, out int N) || N <= 0)
            {
                MessageBox.Show("N ������ ���� ������������� ����� ������.");
                return;
            }

            Random rand = new Random();
            Dictionary<string, int> frequencies = new Dictionary<string, int>();
            foreach (string ev in events)
                frequencies[ev] = 0;

            for (int i = 0; i < N; i++)
            {
                double r = rand.NextDouble();
                double cumulative = 0;
                for (int j = 0; j < probabilities.Count; j++)
                {
                    cumulative += probabilities[j];
                    if (r < cumulative)
                    {
                        frequencies[events[j]]++;
                        break;
                    }
                }
            }

            chartResults.Series.Clear();
            chartResults.ChartAreas[0].AxisX.Title = "�������";
            chartResults.ChartAreas[0].AxisY.Title = "�����������";
            chartResults.ChartAreas[0].AxisY.Maximum = 1.0;
            chartResults.ChartAreas[0].AxisY.Minimum = 0.0;

            Series empiricalSeries = new Series("������������ �����������");
            empiricalSeries.ChartType = SeriesChartType.Column;
            empiricalSeries.Color = System.Drawing.Color.Blue;

            Series theoreticalSeries = new Series("������������� �����������");
            theoreticalSeries.ChartType = SeriesChartType.Column;
            theoreticalSeries.Color = System.Drawing.Color.Red;

            for (int i = 0; i < events.Count; i++)
            {
                double empiricalProb = (double)frequencies[events[i]] / N;
                empiricalSeries.Points.AddXY(events[i], empiricalProb);
                theoreticalSeries.Points.AddXY(events[i], probabilities[i]);
            }

            chartResults.Series.Add(empiricalSeries);
            chartResults.Series.Add(theoreticalSeries);
        }
    }
}