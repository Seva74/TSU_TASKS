using lab11.Models;

namespace lab11.Services
{
    public class CalculationServiceImpl : CalculationService
    {
        Random random = new Random();

        public int getNumber()
        {
            return random.Next(10);
        }

        public string add(int a, int b)
        {
            return (a + b).ToString();
        }

        public string div(int a,int b)
        {
            if (b == 0) return "error";
            else return (a / b).ToString();
        }

        public string mult(int a, int b)
        {
            return (a * b).ToString();
        }

        public string sub(int a, int b)
        {
            return (a - b).ToString();
        }
    }
}
