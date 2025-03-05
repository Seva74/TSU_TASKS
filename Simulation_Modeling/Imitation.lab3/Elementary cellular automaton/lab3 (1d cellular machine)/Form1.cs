using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace lab3__1d_cellular_machine_
{
    public partial class Form1 : Form
    {
        private Random random = new Random();
        private Dictionary<(bool, bool, bool), bool> currentRule = new Dictionary<(bool, bool, bool), bool>();
        private int lines = 100;
        private int Xpos = 0;
        private readonly Brush whiteBrush = new SolidBrush(Color.White);
        private readonly Brush blackBrush = new SolidBrush(Color.Black);
        private Graphics g;
        private List<bool> currentState;
        private int ruleNumber;
        private bool isFirstRun = true;

        public Form1()
        {
            InitializeComponent();
            g = panel1.CreateGraphics();
            currentState = new List<bool>();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            int.TryParse(rule.Text, out ruleNumber);
            int.TryParse(numberOfLines.Text, out lines);

            if (ruleNumber < 0 || ruleNumber > 255 || lines <= 0)
            {
                MessageBox.Show("Введите правило от 0 до 255 и положительное число клеток.");
                return;
            }

            g.Clear(Color.Black);

            var binary = Convert.ToString(ruleNumber, 2).PadLeft(8, '0').ToCharArray();
            currentRule.Clear();
            currentRule.Add((false, false, false), binary[0] == '1');
            currentRule.Add((false, false, true), binary[1] == '1');
            currentRule.Add((false, true, false), binary[2] == '1');
            currentRule.Add((false, true, true), binary[3] == '1');
            currentRule.Add((true, false, false), binary[4] == '1');
            currentRule.Add((true, false, true), binary[5] == '1');
            currentRule.Add((true, true, false), binary[6] == '1');
            currentRule.Add((true, true, true), binary[7] == '1');

            if (isFirstRun)
            {
                currentState.Clear();
                for (int i = 0; i < lines; i++)
                {
                    currentState.Add(random.NextDouble() >= 0.5);
                }
                isFirstRun = false;
            }

            List<bool> nextState = new List<bool>();
            for (int j = 0; j < lines; j++)
            {
                bool left = currentState[(j - 1 + lines) % lines];
                bool center = currentState[j];
                bool right = currentState[(j + 1) % lines];
                nextState.Add(currentRule[(left, center, right)]);
            }

            currentState = nextState;

            for (int j = 0; j < lines; j++)
            {
                Xpos = 10 + j * 10;
                if (currentState[j])
                {
                    g.FillRectangle(whiteBrush, Xpos, 10, 10, 10);
                }
                else
                {
                    g.FillRectangle(blackBrush, Xpos, 10, 10, 10);
                }
            }
        }

        private void Form1_Load(object sender, EventArgs e)
        {
        }
    }
}