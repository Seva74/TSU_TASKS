using System;
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        int[] values = { 1, 2, 3, 4, 5 };
        double[] probs = { 0.1, 0.5, 0.1, 0.1, 0.2 };

        double theoreticalMean = 0;
        double theoreticalMeanSqr = 0;
        for (int i = 0; i < values.Length; i++)
        {
            theoreticalMean += values[i] * probs[i];
            theoreticalMeanSqr += values[i] * values[i] * probs[i];
        }
        double theoreticalVariance = theoreticalMeanSqr - theoreticalMean * theoreticalMean;

        Random rand = new Random();

        int[] Ns = { 10, 100, 1000, 10000 };

        foreach (int N in Ns)
        {
            List<int> samples = new List<int>();
            for (int j = 0; j < N; j++)
            {
                samples.Add(GenerateSample(values, probs, rand));
            }

            int[] frequencies = new int[values.Length];
            foreach (int sample in samples)
            {
                for (int k = 0; k < values.Length; k++)
                {
                    if (sample == values[k])
                    {
                        frequencies[k]++;
                        break;
                    }
                }
            }

            double[] empProbs = new double[values.Length];
            for (int k = 0; k < values.Length; k++)
            {
                empProbs[k] = (double)frequencies[k] / N;
            }

            double sampleMean = 0;
            foreach (int sample in samples)
            {
                sampleMean += sample;
            }
            sampleMean /= N;

            double sampleVariance = 0;
            foreach (int sample in samples)
            {
                sampleVariance += Math.Pow(sample - sampleMean, 2);
            }
            sampleVariance /= (N - 1);

            double relErrMean = double.NaN;
            if (Math.Abs(theoreticalMean) > 1e-10)
            {
                relErrMean = Math.Abs(sampleMean - theoreticalMean) / Math.Abs(theoreticalMean);
            }

            double relErrVar = double.NaN;
            if (Math.Abs(theoreticalVariance) > 1e-10)
            {
                relErrVar = Math.Abs(sampleVariance - theoreticalVariance) / Math.Abs(theoreticalVariance);
            }

            double chiSquare = 0;
            for (int k = 0; k < values.Length; k++)
            {
                double expected = N * probs[k];
                double observed = frequencies[k];
                if (expected > 0)
                {
                    chiSquare += Math.Pow(observed - expected, 2) / expected;
                }
            }

            Console.WriteLine($"Результаты для N = {N}:");
            Console.WriteLine("Эмпирические вероятности:");
            for (int k = 0; k < values.Length; k++)
            {
                Console.WriteLine($"P({values[k]}) = {empProbs[k]:F4} (теоретическая: {probs[k]:F4})");
            }
            Console.WriteLine($"Выборочное среднее: {sampleMean:F4} (теоретическое: {theoreticalMean:F4}), относительная погрешность: {relErrMean:F4}");
            Console.WriteLine($"Выборочная дисперсия: {sampleVariance:F4} (теоретическая: {theoreticalVariance:F4}), относительная погрешность: {relErrVar:F4}");
            Console.WriteLine($"Статистика хи-квадрат: {chiSquare:F4}\n");
        }
    }

    static int GenerateSample(int[] values, double[] probs, Random rand)
    {
        double U = rand.NextDouble();
        double cumulative = 0;
        for (int i = 0; i < values.Length; i++)
        {
            cumulative += probs[i];
            if (U < cumulative)
            {
                return values[i];
            }
        }
        return values[values.Length - 1];
    }
}