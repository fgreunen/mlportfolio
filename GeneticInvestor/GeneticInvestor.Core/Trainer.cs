using System;
using System.Collections.Generic;
using System.Linq;

namespace GeneticInvestor.Core
{
    public static class Trainer
    {
        private const int RUNS = 1;
        private static Random random = new Random();
        private static float GetRandomNumber(double minimum, double maximum)
        {
            return (float)(random.NextDouble() * (maximum - minimum) + minimum);
        }

        private static Member _Train(List<AllocationRun> examples, float[] returns, float[][] values, Func<float[], float> fitnessFunc, int iterations)
        {
            //Func<float[], float> fitnessFunc = (chromosome) =>
            //{
            //    var valueWeights = new float[returns.Length];
            //    for (int i = 0; i < returns.Length; i++)
            //    {
            //        float weight = 0;
            //        for (var j = 0; j < values[i].Length; j++)
            //            weight += chromosome[j] * values[i][j];
            //        valueWeights[i] = weight;
            //    }
            //    var total = valueWeights.Sum();

            //    float fitnessValue = 0;
            //    for (var i = 0; i < returns.Length; i++)
            //        fitnessValue += returns[i] * (valueWeights[i] / total);
            //    return fitnessValue;
            //};

            var min = 0;
            var max = 1;
            var popSize = 50;
            float[][] chromosomes = new float[popSize][];
            for (var i = 0; i < chromosomes.Length; i++)
            {
                float[] chromosome = new float[]
                {
                    GetRandomNumber(min, max),
                    GetRandomNumber(min, max),
                    GetRandomNumber(min, max),
                    GetRandomNumber(min, max),
                    GetRandomNumber(min, max),
                };
                chromosomes[i] = chromosome;
            }
            float mutationRate = 0.03f;
            float mutation = 0.25f;
            bool allowNegative = false;

            var population = new Population(chromosomes, fitnessFunc, mutationRate, mutation, allowNegative);
            for (int i = 0; i < iterations; i++)
                population.Iterate();

            return population.BestMember;
        }

        public static List<Member> Train(List<AllocationRun> examples, float[] returns, float[][] values, Func<float[], float> fitnessFunc, int iterations = 1000)
        {
            var results = new List<Member>();
            for (int i = 0; i < RUNS; i++)
                results.Add(_Train(examples, returns, values, fitnessFunc, iterations));
            return results;
        }
    }
}