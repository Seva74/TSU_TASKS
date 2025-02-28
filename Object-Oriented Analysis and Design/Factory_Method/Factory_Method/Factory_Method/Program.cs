using System;

// Абстрактный класс для учебных материалов
abstract class LearningMaterial
{
    public string Title { get; set; }
    public string Content { get; set; }

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

class Program
{
    static void Main()
    {
        Teacher teacher = new Teacher("Marina Litovchenko");

        LearningMaterialFactory testFactory = new TestFactory();
        LearningMaterialFactory taskFactory = new PracticalTaskFactory();
        LearningMaterialFactory lectureFactory = new LectureFactory();

        LearningMaterial test = teacher.CreateMaterial(testFactory);
        LearningMaterial task = teacher.CreateMaterial(taskFactory);
        LearningMaterial lecture = teacher.CreateMaterial(lectureFactory);

        test.View();
        task.View();
        lecture.View();
    }
}
