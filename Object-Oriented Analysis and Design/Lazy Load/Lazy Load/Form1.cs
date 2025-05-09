using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Windows.Forms;

namespace LazyLoadDemo
{
    public partial class Form1 : Form
    {
        private List<NewsItem> newsItems;

        public Form1()
        {
            InitializeComponent();
            newsItems = new List<NewsItem>
            {
                new NewsItem("News 1", "news1.txt"),
                new NewsItem("News 2", "news2.txt"),
                new NewsItem("News 3", "news3.txt"),
            };
            listBox1.DisplayMember = "Title";
            listBox1.DataSource = newsItems;
        }

        private void listBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (listBox1.SelectedItem is NewsItem selectedItem)
            {
                statusLabel.Text = "Загрузка...";
                Application.DoEvents(); // Обновление интерфейса
                string content = selectedItem.Content;
                textBox1.Text = content;
                statusLabel.Text = "";
            }
        }

        public class NewsItem
        {
            public string Title { get; set; }
            public string Filename { get; set; }
            private Lazy<string> _content;

            public string Content
            {
                get { return _content.Value; }
            }

            public NewsItem(string title, string filename)
            {
                Title = title;
                Filename = filename;
                _content = new Lazy<string>(() =>
                {
                    try
                    {
                        Console.WriteLine($"Загрузка содержимого из {filename}...");
                        Thread.Sleep(2000);
                        string content = File.ReadAllText(filename);
                        Console.WriteLine($"Содержимое загружено из {filename}.");
                        return content;
                    }
                    catch (Exception ex)
                    {
                        return $"Ошибка загрузки содержимого: {ex.Message}";
                    }
                });
            }
        }
    }
}