using System.ComponentModel.DataAnnotations;

namespace lab13.Models
{
    public record QuizQuestion(int firstNumber, int secondNumber, string operation, int answer);
}
