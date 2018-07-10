using System;

namespace GeneticInvestor.Core
{
    public class Member
    {
        private const double CROSSOVER_CHANCE = 0.95;
        static Random random = new Random();

        public int Rank { get; set; }
        public double[] Chromosome { get; private set; }
        private readonly Func<double[], double> _fitnessFunction;

        public Member(double[] chromosome, Func<double[], double> fitnessFunction)
        {
            Chromosome = chromosome;
            _fitnessFunction = fitnessFunction;
        }

        public double Fitness()
        {
            return _fitnessFunction(Chromosome);
        }

        public Member Breed(Member other)
        {
            double[] newChromosome = new double[Chromosome.Length];
            for (var i = 0; i < Chromosome.Length; i++)
                newChromosome[i] = (0.55 * Chromosome[i] + 0.45 * other.Chromosome[i]);
            return new Member(newChromosome, _fitnessFunction);
        }
    }
}