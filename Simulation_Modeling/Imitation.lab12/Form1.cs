using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;
using System.Diagnostics.CodeAnalysis;

public partial class Form1 : Form
{
    private Random rand;
    private double[] leaving_rates = { 0.4, 0.8, 0.5 };
    private List<(int state, double prob)>[] transitions;
    private int current_state;
    private double current_time;
    private double last_update_time;
    private double next_transition_time;
    private double[] durations;
    private System.Windows.Forms.Timer timer;
    private string[] state_names = { "Ясно", "Облачно", "Пасмурно" };
    private Color[] colors = { Color.SkyBlue, Color.LightGray, Color.DarkGray };
    private bool isRunning;

    public Form1()
    {
        InitializeComponent();
        rand = new Random();
        transitions = new List<(int, double)>[3];
        transitions[0] = new List<(int, double)> { (1, 0.75), (2, 0.25) };
        transitions[1] = new List<(int, double)> { (0, 0.5), (2, 0.5) };
        transitions[2] = new List<(int, double)> { (0, 0.2), (1, 0.8) };
        durations = new double[3] { 0, 0, 0 };
        timer = new System.Windows.Forms.Timer();
        timer.Interval = 100;
        timer.Tick += Timer_Tick;
        lblWeather.Text = state_names[0];
        lblTime.Text = "0 дней";
        lblP1.Text = "P(Ясно): 0.0000";
        lblP2.Text = "P(Облачно): 0.0000";
        lblP3.Text = "P(Пасмурно): 0.0000";
        panelWeather.BackColor = colors[0];
        isRunning = false;
        btnControl.Text = "Старт";
    }

    private void ResetSimulation()
    {
        current_state = 0;
        current_time = 0;
        last_update_time = 0;
        next_transition_time = -Math.Log(rand.NextDouble()) / leaving_rates[current_state];
        durations = new double[3] { 0, 0, 0 };
        lblWeather.Text = state_names[0];
        lblTime.Text = "0 дней";
        lblP1.Text = "P(Ясно): 0.0000";
        lblP2.Text = "P(Облачно): 0.0000";
        lblP3.Text = "P(Пасмурно): 0.0000";
        panelWeather.BackColor = colors[0];
    }

    private void btnControl_Click(object sender, EventArgs e)
    {
        if (!isRunning)
        {
            if (btnControl.Text == "Старт")
            {
                ResetSimulation();
            }
            timer.Start();
            isRunning = true;
            btnControl.Text = "Пауза";
        }
        else
        {
            timer.Stop();
            isRunning = false;
            btnControl.Text = "Продолжить";
        }
    }

    private void Timer_Tick([NotNull] object sender, EventArgs e)
    {
        double dt = 0.2;
        current_time += dt;

        while (next_transition_time <= current_time)
        {
            durations[current_state] += next_transition_time - last_update_time;
            last_update_time = next_transition_time;

            double U = rand.NextDouble();
            double cum_prob = 0;
            foreach (var trans in transitions[current_state])
            {
                cum_prob += trans.prob;
                if (U < cum_prob)
                {
                    current_state = trans.state;
                    break;
                }
            }

            double delta_t = -Math.Log(rand.NextDouble()) / leaving_rates[current_state];
            next_transition_time = last_update_time + delta_t;
        }

        durations[current_state] += current_time - last_update_time;
        last_update_time = current_time;

        lblWeather.Text = state_names[current_state];
        lblTime.Text = $"{(int)current_time} дней";
        if (current_time > 0)
        {
            double total = current_time;
            lblP1.Text = $"P(Ясно): {(durations[0] / total):F4}";
            lblP2.Text = $"P(Облачно): {(durations[1] / total):F4}";
            lblP3.Text = $"P(Пасмурно): {(durations[2] / total):F4}";
        }
        panelWeather.BackColor = colors[current_state];

        if (current_time >= 50)
        {
            timer.Stop();
            isRunning = false;
            btnControl.Text = "Старт";
            MessageBox.Show("Симуляция завершена.\n" +
                            $"Итоговое P(Ясно): {durations[0] / current_time:F4}\n" +
                            $"Итоговое P(Облачно): {durations[1] / current_time:F4}\n" +
                            $"Итоговое P(Пасмурно): {durations[2] / current_time:F4}");
        }
    }
}