namespace lab13.Models
{
    public class Quiz
    {
        public List<QuizQuestion> quizQuestions { get; }
        public List<QuizQuestionAnswer> quizQuestionAnswers { get; }

        public Quiz()
        {
            this.quizQuestions = new();
            this.quizQuestionAnswers = new();
        }

        public void addQuestion(QuizQuestion question)
        {
            quizQuestions.Add(question);
        }

        public void addAnswerQuestion(QuizQuestionAnswer answer)
        {
            quizQuestionAnswers.Add(answer);
        }

        public void reset()
        {
            quizQuestionAnswers.Clear();
            quizQuestions.Clear();
        }
    }
}
