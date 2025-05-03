using System;

public static class RandomHelper
{
    private static Random rand = new Random();

    public static double GetUniform()
    {
        return rand.NextDouble();
    }

    public static int GetPoisson(double lambda)
    {
        if (lambda < 30.0)
        {
            return PoissonSmall(lambda);
        }
        else
        {
            double mean = lambda;
            double variance = lambda;
            double stddev = Math.Sqrt(variance);
            double normalSample = mean + stddev * BoxMullerTransform();
            return (int)Math.Round(normalSample);
        }
    }

    private static int PoissonSmall(double lambda)
    {
        double p = 1.0, L = Math.Exp(-lambda);
        int k = 0;
        do
        {
            k++;
            p *= GetUniform();
        } while (p > L);
        return k - 1;
    }

    private static double BoxMullerTransform()
    {
        double u1 = GetUniform();
        double u2 = GetUniform();
        return Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Cos(2.0 * Math.PI * u2);
    }
}

class Program
{
    static void Main(string[] args)
    {
        double lambda_A = 100;
        double lambda_B = 90;
        int numGames = 100;
        int wins_A = 0;
        int wins_B = 0;
        int ties = 0;

        for (int i = 0; i < numGames; i++)
        {
            int points_A = RandomHelper.GetPoisson(lambda_A);
            int points_B = RandomHelper.GetPoisson(lambda_B);

            if (points_A > points_B)
                wins_A++;
            else if (points_B > points_A)
                wins_B++;
            else
                ties++;
        }

        Console.WriteLine($"Результаты симуляции {numGames} матчей:");
        Console.WriteLine($"Победы команды A: {wins_A}");
        Console.WriteLine($"Победы команды B: {wins_B}");
        Console.WriteLine($"Ничьи: {ties}");
    }
}