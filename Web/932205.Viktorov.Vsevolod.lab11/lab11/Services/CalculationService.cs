namespace lab11.Services
{
    public interface CalculationService
    {
        public int getNumber();

        public string add(int a, int b);

        public string div(int a, int b);

        public string mult(int a, int b);

        public string sub(int a, int b);
    }
}
