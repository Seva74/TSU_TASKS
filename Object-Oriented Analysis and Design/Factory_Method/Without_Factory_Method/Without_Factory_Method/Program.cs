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
}

class PracticalTask : LearningMaterial
{
    public override void View() => Console.WriteLine($"Viewing practical task: {Title}");
}

class Lecture : LearningMaterial
{
    public override void View() => Console.WriteLine($"Viewing lecture: {Title}");
}

// Класс преподавателя
class Teacher
{
    public string Name { get; set; }

    public Teacher(string name)
    {
        Name = name;
    }

    // Метод для создания учебного материала напрямую
    public LearningMaterial CreateTest()
    {
        return new Test { Title = "Sample Test", Content = "Test Content" };
    }

    public LearningMaterial CreatePracticalTask()
    {
        return new PracticalTask { Title = "Sample Task", Content = "Task Content" };
    }

    public LearningMaterial CreateLecture()
    {
        return new Lecture { Title = "Sample Lecture", Content = "Lecture Content" };
    }
}

class Program
{
    static void Main()
    {
        Teacher teacher = new Teacher("Marina Litovchenko");

        // Преподаватель вручную создаёт материалы
        LearningMaterial test = teacher.CreateTest();
        LearningMaterial task = teacher.CreatePracticalTask();
        LearningMaterial lecture = teacher.CreateLecture();

        test.View();
        task.View();
        lecture.View();
    }
}
