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
        private Label lblTitleInput = new Label(); // ����� ������� ��� ���� �����
        private TextBox txtTitleInput = new TextBox(); // ����� ���� �����
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
            // ��������� �����
            this.Text = "������� ���������";
            this.Width = 500;
            this.Height = 450; // �������� ������, ����� �������� ����� ����

            // ���� ����� ��������
            lblTitleInput.Text = "�������� ���������:";
            lblTitleInput.Location = new System.Drawing.Point(20, 20);
            lblTitleInput.Width = 100;

            txtTitleInput.Width = 340;
            txtTitleInput.Location = new System.Drawing.Point(120, 20);
            txtTitleInput.Text = "������� ��������"; // �������� �� ���������

            // ������� � ������ ��� �����
            lblTest.Text = "����:";
            lblTest.Location = new System.Drawing.Point(20, 60);
            lblTest.Width = 100;

            btnCreateTest.Text = "�������";
            btnCreateTest.Location = new System.Drawing.Point(120, 60);
            btnCreateTest.Width = 100;
            btnCreateTest.Click += BtnCreateTest_Click;

            btnTakeTest.Text = "������";
            btnTakeTest.Location = new System.Drawing.Point(230, 60);
            btnTakeTest.Width = 100;
            btnTakeTest.Click += BtnTakeTest_Click;

            // ������� � ������ ��� ������������ ������
            lblTask.Text = "������������ ������:";
            lblTask.Location = new System.Drawing.Point(20, 100);
            lblTask.Width = 100;

            btnCreateTask.Text = "�������";
            btnCreateTask.Location = new System.Drawing.Point(120, 100);
            btnCreateTask.Width = 100;
            btnCreateTask.Click += BtnCreateTask_Click;

            btnSubmitTask.Text = "�����";
            btnSubmitTask.Location = new System.Drawing.Point(230, 100);
            btnSubmitTask.Width = 100;
            btnSubmitTask.Click += BtnSubmitTask_Click;

            // ������� � ������ ��� ������
            lblLecture.Text = "������:";
            lblLecture.Location = new System.Drawing.Point(20, 140);
            lblLecture.Width = 100;

            btnCreateLecture.Text = "�������";
            btnCreateLecture.Location = new System.Drawing.Point(120, 140);
            btnCreateLecture.Width = 100;
            btnCreateLecture.Click += BtnCreateLecture_Click;

            btnDownloadLecture.Text = "�������";
            btnDownloadLecture.Location = new System.Drawing.Point(230, 140);
            btnDownloadLecture.Width = 100;
            btnDownloadLecture.Click += BtnDownloadLecture_Click;

            // ��������� ���� ��� ������
            txtOutput.Multiline = true;
            txtOutput.Width = 440;
            txtOutput.Height = 200;
            txtOutput.Location = new System.Drawing.Point(20, 180);
            txtOutput.ReadOnly = true;

            // ���������� ��������� �� �����
            this.Controls.Add(lblTitleInput);
            this.Controls.Add(txtTitleInput);
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

        private LearningMaterial? lastCreatedMaterial;

        private void BtnCreateTest_Click(object? sender, EventArgs e)
        {
            LearningMaterialFactory factory = new TestFactory();
            lastCreatedMaterial = teacher.CreateMaterial(factory);
            lastCreatedMaterial.Title = string.IsNullOrEmpty(txtTitleInput.Text) ? "Sample Test" : txtTitleInput.Text;
            lastCreatedMaterial.View();
            txtOutput.Text += $"������: {lastCreatedMaterial.Title}\r\n";
        }

        private void BtnCreateTask_Click(object? sender, EventArgs e)
        {
            LearningMaterialFactory factory = new PracticalTaskFactory();
            lastCreatedMaterial = teacher.CreateMaterial(factory);
            lastCreatedMaterial.Title = string.IsNullOrEmpty(txtTitleInput.Text) ? "Sample Task" : txtTitleInput.Text;
            lastCreatedMaterial.View();
            txtOutput.Text += $"������: {lastCreatedMaterial.Title}\r\n";
        }

        private void BtnCreateLecture_Click(object? sender, EventArgs e)
        {
            LearningMaterialFactory factory = new LectureFactory();
            lastCreatedMaterial = teacher.CreateMaterial(factory);
            lastCreatedMaterial.Title = string.IsNullOrEmpty(txtTitleInput.Text) ? "Sample Lecture" : txtTitleInput.Text;
            lastCreatedMaterial.View();
            txtOutput.Text += $"������: {lastCreatedMaterial.Title}\r\n";
        }

        private void BtnTakeTest_Click(object? sender, EventArgs e)
        {
            if (lastCreatedMaterial is Test test)
            {
                test.TakeTest();
                txtOutput.Text += $"������� ����: {test.Title}\r\n";
            }
            else
            {
                txtOutput.Text += "������� �������� ����!\r\n";
            }
        }

        private void BtnSubmitTask_Click(object? sender, EventArgs e)
        {
            if (lastCreatedMaterial is PracticalTask task)
            {
                task.SubmitForReview();
                txtOutput.Text += $"����� ������: {task.Title}\r\n";
            }
            else
            {
                txtOutput.Text += "������� �������� ������!\r\n";
            }
        }

        private void BtnDownloadLecture_Click(object? sender, EventArgs e)
        {
            if (lastCreatedMaterial is Lecture lecture)
            {
                lecture.DownloadPresentation();
                txtOutput.Text += $"������� ������: {lecture.Title}\r\n";
            }
            else
            {
                txtOutput.Text += "������� �������� ������!\r\n";
            }
        }
    }

    // ����������� ����� ��� ������� ����������
    abstract class LearningMaterial
    {
        public string Title { get; set; } = string.Empty;
        public string Content { get; set; } = string.Empty;

        public abstract void View();
    }

    // ���������� ������ ������� ����������
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

    // ����������� �������
    abstract class LearningMaterialFactory
    {
        public abstract LearningMaterial CreateMaterial();
    }

    // ���������� �������
    class TestFactory : LearningMaterialFactory
    {
        public override LearningMaterial CreateMaterial() => new Test { Content = "Test Content" }; // Title ������ ������� ����� ���� �����
    }

    class PracticalTaskFactory : LearningMaterialFactory
    {
        public override LearningMaterial CreateMaterial() => new PracticalTask { Content = "Task Content" };
    }

    class LectureFactory : LearningMaterialFactory
    {
        public override LearningMaterial CreateMaterial() => new Lecture { Content = "Lecture Content" };
    }

    // ����� �������������
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