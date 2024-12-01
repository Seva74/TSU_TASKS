using lab12.Models;

namespace lab12.Services
{
    public interface CalculationService
    {
        public string calc(int a, int b, string operation);

        public string calc(CalculationExpressionModel calculationExpression);

        public string add(int a, int b);

        public string div(int a, int b);

        public string mult(int a, int b);

        public string sub(int a, int b);
    }
}
