using lab13.Models;

namespace lab13.Services
{
    public interface IQuizService
    {
        public Quiz getCurrentQuiz();
        public QuizQuestion addNewQuestion(Quiz quiz);
        public QuizQuestion getCurrentQuestion(Quiz quiz);
        public void answerLastQuestion(QuizQuestionAnswer answer, Quiz quiz);
        public void finish(Quiz quiz);
    }
}
