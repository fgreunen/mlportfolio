using System;
using System.Collections.Generic;
using System.Linq;

namespace GeneticInvestor.Core
{
    public static class Trainer
    {
        private const int RUNS = 2;
        private static Random random = new Random();
        private static double GetRandomNumber(double minimum, double maximum)
        {
            return (random.NextDouble() * (maximum - minimum) + minimum);
        }

        private static Member _Train(double[] returns, double[][] values, Func<double[], double> fitnessFunc, int iterations)
        {
            var min = 1;
            var max = 1.4;
            var popSize = 90;
            double[][] chromosomes = new double[popSize][];
            for (var i = 0; i < chromosomes.Length; i++)
            {
                double[] chromosome = new double[values.Length];
                for (int j = 0; j < values.Length; j++)
                    chromosome[j] = GetRandomNumber(min, max);
                chromosomes[i] = chromosome;
            }
            double mutationRate = 0.1f;
            double mutation = 0.08f;
            bool allowNegative = false;

            var population = new Population(chromosomes, fitnessFunc, mutationRate, mutation, allowNegative);
            for (int i = 0; i < iterations; i++)
                population.Iterate();

            return population.BestMember;
        }

        public static Member Train(double[] returns, double[][] values, Func<double[], double> fitnessFunc, int iterations = 1000)
        {
            var results = new List<Member>();
            for (int i = 0; i < RUNS; i++)
                results.Add(_Train(returns, values, fitnessFunc, iterations));

            results.ForEach(x =>
            {
                double total = x.Chromosome.Sum();
                for (int i = 0; i < x.Chromosome.Length; i++)
                    x.Chromosome[i] = x.Chromosome[i] / total;
            });

            double[] averageChromosome = new double[values.Length];
            for (int i = 0; i < values.Length; i++)
                averageChromosome[i] = results.Select(x => x.Chromosome[i]).Average();

            return new Member(averageChromosome, fitnessFunc);
        }
    }
}