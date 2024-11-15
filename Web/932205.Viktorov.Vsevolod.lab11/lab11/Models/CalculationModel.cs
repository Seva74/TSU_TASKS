namespace lab11.Models
{
    public class CalculationModel
    {
        public int firstNumber { get; set; }
        public int secondNumber { get; set; }


        public CalculationModel()
        {
            var rand = new Random(DateTime.Now.Millisecond);
            firstNumber = rand.Next(10);
            secondNumber = rand.Next(10);
        }

        public CalculationModel(int a, int b) 
        {
            firstNumber = a;
            secondNumber = b;
        }
    }
}
