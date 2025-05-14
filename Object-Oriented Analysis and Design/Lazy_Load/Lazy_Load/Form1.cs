using System;
using System.Drawing;
using System.Windows.Forms;
using System.Collections.Generic;
using System.IO;
using System.Threading;

namespace LazyLoadDemo
{
    public class NewsItem
    {
        public string Title { get; }
        public string Filename { get; }
        private string content;
        private bool isLoaded;

        public NewsItem(string title, string filename)
        {
            Title = title;
            Filename = filename;
            isLoaded = false;
        }

        public string GetContent()
        {
            if (!isLoaded)
            {
                Console.WriteLine($"�������� ����������� �� {Filename}...");
                Thread.Sleep(2000);
                try
                {
                    content = File.ReadAllText(Filename);
                }
                catch (Exception ex)
                {
                    content = $"<html><body><h1>������ ��������</h1><p>{ex.Message}</p></body></html>";
                }
                isLoaded = true;
                Console.WriteLine($"���������� ��������� �� {Filename}.");
            }
            return content;
        }
    }

    public class NewsLibrary
    {
        private readonly List<NewsItem> newsItems; //����������

        public NewsLibrary()
        {
            newsItems = new List<NewsItem> //����������
            {
                new NewsItem("����� �������� � �������", "news1.html"),
                new NewsItem("������������� ������� �� 2025 ���", "news2.html"),
                new NewsItem("��������� ���� �� �������", "news3.html"),
            };
        }

        public List<NewsItem> GetNewsItems()
        {
            return newsItems;
        }
    }

    public partial class Form1 : Form
    {
        private readonly NewsLibrary newsLibrary; //����������

        public Form1()
        {
            InitializeComponent(); //����������
            newsLibrary = new NewsLibrary();
            listView1.Items.Clear();
            foreach (var item in newsLibrary.GetNewsItems())
            {
                listView1.Items.Add(item.Title);
            }
        }

        private void listView1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (listView1.SelectedIndices.Count > 0)
            {
                int index = listView1.SelectedIndices[0];
                var newsItem = newsLibrary.GetNewsItems()[index];
                statusLabel.Text = "��������...";
                Application.DoEvents();
                string content = newsItem.GetContent(); //�������������
                webBrowser1.DocumentText = content;
                statusLabel.Text = "";
            }
        }

        private void headerPanel_Paint(object sender, PaintEventArgs e)
        {
            if (headerPanel.Width <= 0 || headerPanel.Height <= 0)
                return;

            using (var brush = new System.Drawing.Drawing2D.LinearGradientBrush(
                headerPanel.ClientRectangle,
                Color.FromArgb(0, 120, 215),
                Color.FromArgb(0, 80, 180),
                System.Drawing.Drawing2D.LinearGradientMode.Horizontal))
            {
                e.Graphics.FillRectangle(brush, headerPanel.ClientRectangle);
            }
        }
    }
}