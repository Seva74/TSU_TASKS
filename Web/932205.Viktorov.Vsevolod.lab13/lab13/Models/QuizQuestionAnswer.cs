using System.ComponentModel.DataAnnotations;

namespace lab13.Models
{
    public class QuizQuestionAnswer
    {
        [Required]
        public int answer { get; set; }

        public QuizQuestionAnswer(int answer)
        {
            this.answer = answer;
        }
    }
}
