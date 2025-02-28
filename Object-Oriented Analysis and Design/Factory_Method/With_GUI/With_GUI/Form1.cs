using System;
using System.Windows.Forms;

namespace With_GUI
{
    public partial class Form1 : Form
    {
        private Button btnCreateTest = new Button();
        private Button btnCreateTask = new Button();
        private Button btnCreateLecture = new Button();
        private Button btnTakeTest = new Button();
        private Button btnSubmitTask = new Button();
        private Button btnDownloadLecture = new Button();
        private Label lblTest = new Label();
        private Label lblTask = new Label();
        private Label lblLecture = new Label();
        private TextBox txtOutput = new TextBox();
        private Teacher teacher;

        public Form1()
        {
            InitializeComponent();
            InitializeCustomComponents();
            teacher = new Teacher("Marina Litovchenko");
        }

        private void InitializeCustomComponents()
        {
            // Заголовок формы
            this.Text = "Учебные материалы";
            this.Width = 500;
            this.Height = 400;

            // Подпись и кнопка для теста
            lblTest.Text = "Тест:";
            lblTest.Location = new System.Drawing.Point(20, 20);
            lblTest.Width = 100;

            btnCreateTest.Text = "Создать";
            btnCreateTest.Location = new System.Drawing.Point(120, 20);
            btnCreateTest.Width = 100;
            btnCreateTest.Click += BtnCreateTest_Click;

            btnTakeTest.Text = "Пройти";
            btnTakeTest.Location = new System.Drawing.Point(230, 20);
            btnTakeTest.Width = 100;
            btnTakeTest.Click += BtnTakeTest_Click;

            // Подпись и кнопка для практической задачи
            lblTask.Text = "Практическая задача:";
            lblTask.Location = new System.Drawing.Point(20, 60);
            lblTask.Width = 100;

            btnCreateTask.Text = "Создать";
            btnCreateTask.Location = new System.Drawing.Point(120, 60);
            btnCreateTask.Width = 100;
            btnCreateTask.Click += BtnCreateTask_Click;

            btnSubmitTask.Text = "Сдать";
            btnSubmitTask.Location = new System.Drawing.Point(230, 60);
            btnSubmitTask.Width = 100;
            btnSubmitTask.Click += BtnSubmitTask_Click;

            // Подпись и кнопка для лекции
            lblLecture.Text = "Лекция:";
            lblLecture.Location = new System.Drawing.Point(20, 100);
            lblLecture.Width = 100;

            btnCreateLecture.Text = "Создать";
            btnCreateLecture.Location = new System.Drawing.Point(120, 100);
            btnCreateLecture.Width = 100;
            btnCreateLecture.Click += BtnCreateLecture_Click;

            btnDownloadLecture.Text = "Скачать";
            btnDownloadLecture.Location = new System.Drawing.Point(230, 100);
            btnDownloadLecture.Width = 100;
            btnDownloadLecture.Click += BtnDownloadLecture_Click;

            // Текстовое поле для вывода
            txtOutput.Multiline = true;
            txtOutput.Width = 440;
            txtOutput.Height = 200;
            txtOutput.Location = new System.Drawing.Point(20, 140);
            txtOutput.ReadOnly = true;

            // Добавление элементов на форму
            this.Controls.Add(lblTest);
            this.Controls.Add(btnCreateTest);
            this.Controls.Add(btnTakeTest);
            this.Controls.Add(lblTask);
            this.Controls.Add(btnCreateTask);
            this.Controls.Add(btnSubmitTask);
            this.Controls.Add(lblLecture);
            this.Controls.Add(btnCreateLecture);
            this.Controls.Add(btnDownloadLecture);
            this.Controls.Add(txtOutput);
        }

        private LearningMaterial? lastCreatedMaterial; // Храним последний созданный материал

        private void BtnCreateTest_Click(object? sender, EventArgs e)
        {
            LearningMaterialFactory factory = new TestFactory();
            lastCreatedMaterial = teacher.CreateMaterial(factory);
            lastCreatedMaterial.View();
            txtOutput.Text += $"Создан: {lastCreatedMaterial.Title}\r\n";
        }

        private void BtnCreateTask_Click(object? sender, EventArgs e)
        {
            LearningMaterialFactory factory = new PracticalTaskFactory();
            lastCreatedMaterial = teacher.CreateMaterial(factory);
            lastCreatedMaterial.View();
            txtOutput.Text += $"Создан: {lastCreatedMaterial.Title}\r\n";
        }

        private void BtnCreateLecture_Click(object? sender, EventArgs e)
        {
            LearningMaterialFactory factory = new LectureFactory();
            lastCreatedMaterial = teacher.CreateMaterial(factory);
            lastCreatedMaterial.View();
            txtOutput.Text += $"Создан: {lastCreatedMaterial.Title}\r\n";
        }

        private void BtnTakeTest_Click(object? sender, EventArgs e)
        {
            if (lastCreatedMaterial is Test test)
            {
                test.TakeTest();
                txtOutput.Text += $"Пройден тест: {test.Title}\r\n";
            }
            else
            {
                txtOutput.Text += "Сначала создайте тест!\r\n";
            }
        }

        private void BtnSubmitTask_Click(object? sender, EventArgs e)
        {
            if (lastCreatedMaterial is PracticalTask task)
            {
                task.SubmitForReview();
                txtOutput.Text += $"Сдана задача: {task.Title}\r\n";
            }
            else
            {
                txtOutput.Text += "Сначала создайте задачу!\r\n";
            }
        }

        private void BtnDownloadLecture_Click(object? sender, EventArgs e)
        {
            if (lastCreatedMaterial is Lecture lecture)
            {
                lecture.DownloadPresentation();
                txtOutput.Text += $"Скачана лекция: {lecture.Title}\r\n";
            }
            else
            {
                txtOutput.Text += "Сначала создайте лекцию!\r\n";
            }
        }
    }

    // Абстрактный класс для учебных материалов
    abstract class LearningMaterial
    {
        public string Title { get; set; } = string.Empty;
        public string Content { get; set; } = string.Empty;

        public abstract void View();
    }

    // Конкретные классы учебных материалов
    class Test : LearningMaterial
    {
        public override void View() => Console.WriteLine($"Viewing test: {Title}");
        public void TakeTest() => Console.WriteLine("Taking the test");
    }

    class PracticalTask : LearningMaterial
    {
        public override void View() => Console.WriteLine($"Viewing practical task: {Title}");
        public void SubmitForReview() => Console.WriteLine("Submitting practical task for review");
    }

    class Lecture : LearningMaterial
    {
        public override void View() => Console.WriteLine($"Viewing lecture: {Title}");
        public void DownloadPresentation() => Console.WriteLine("Downloading lecture presentation");
    }

    // Абстрактная фабрика
    abstract class LearningMaterialFactory
    {
        public abstract LearningMaterial CreateMaterial();
    }

    // Конкретные фабрики
    class TestFactory : LearningMaterialFactory
    {
        public override LearningMaterial CreateMaterial() => new Test { Title = "Sample Test", Content = "Test Content" };
    }

    class PracticalTaskFactory : LearningMaterialFactory
    {
        public override LearningMaterial CreateMaterial() => new PracticalTask { Title = "Sample Task", Content = "Task Content" };
    }

    class LectureFactory : LearningMaterialFactory
    {
        public override LearningMaterial CreateMaterial() => new Lecture { Title = "Sample Lecture", Content = "Lecture Content" };
    }

    // Класс преподавателя
    class Teacher
    {
        public string Name { get; set; }

        public Teacher(string name)
        {
            Name = name;
        }

        public LearningMaterial CreateMaterial(LearningMaterialFactory factory)
        {
            return factory.CreateMaterial();
        }
    }
}