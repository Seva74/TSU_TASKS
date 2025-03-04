using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace lab4__2d_cellular_machine_
{
    public partial class Form1 : Form
    {
        Random random = new Random();
        private readonly Brush whiteBrush = new SolidBrush(Color.White);
        private readonly Brush blackBrush = new SolidBrush(Color.Black);
        private Graphics g;
        private int Ypos = 0;
        private int Xpos = 0;

        // Массивы смещений для соседей
        private readonly int[] rowOffsets = { -1, -1, -1, 0, 0, 1, 1, 1 };
        private readonly int[] colOffsets = { -1, 0, 1, -1, 1, -1, 0, 1 };

        private bool[,] area1 = new bool[100, 100];
        private bool[,] nextGeneration = new bool[100, 100];
        private List<Point> updated = new List<Point>();
        public Form1()
        {
            InitializeComponent();
            g = panel1.CreateGraphics();
            for (int i = 0; i < 100; i++)
                for (int j = 0; j < 100; j++)
                    area1[i, j] = true;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            g.FillRectangle(whiteBrush, 10, 10, 1000, 1000);
            for (int i = 0; i < 6; i++)
                for (int j = 0; j < 6; j++)
                {
                    updated.Add(new Point(48 + i, 48 + j));
                    area1[48 + i, 48 + j] = random.NextDouble() < 0.3;
                    g.FillRectangle(area1[48 + i, 48 + j] ? blackBrush : whiteBrush, 490 + 10 * i, 490 + 10 * j, 10, 10);
                }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            nextGeneration.Initialize();
            int aliveNeighborsNum;
            bool newState;
            for (int row = 0; row < 100; row++)
                for (int col = 0; col < 100; col++)
                {
                    //посчитали живых соседей
                    aliveNeighborsNum = 0;
                    for (int k = 0; k < 8; k++)
                    {
                        int neighborRow = (row + rowOffsets[k] + 100) % 100;
                        int neighborCol = (col + colOffsets[k] + 100) % 100;
                        if (area1[neighborRow, neighborCol]) aliveNeighborsNum++;
                    }
                    // Правила игры "Жизнь"
                    newState = area1[row, col] ? aliveNeighborsNum == 2 || aliveNeighborsNum == 3 : aliveNeighborsNum == 3;

                    //если состояние клетки изменилось - перекрашиваем 
                    if (area1[row, col] != newState)
                        g.FillRectangle(newState ? blackBrush : whiteBrush, 10 + 10 * row, 10 + 10 * col, 10, 10);
                    //Запоминаем клетку нового поколения
                    nextGeneration[row, col] = newState;
                    
                }
            area1 = nextGeneration;
            
                    
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }
    }
}
