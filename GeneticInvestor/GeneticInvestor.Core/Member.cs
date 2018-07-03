using System;

namespace GeneticInvestor.Core
{
    public class Member
    {
        private const double CROSSOVER_CHANCE = 0.9;
        static Random random = new Random();

        public int Rank { get; set; }
        public float[] Chromosome { get; private set; }
        private readonly Func<float[], float> _fitnessFunction;

        public Member(float[] chromosome, Func<float[], float> fitnessFunction)
        {
            Chromosome = chromosome;
            _fitnessFunction = fitnessFunction;
        }

        public float Fitness()
        {
            return _fitnessFunction(Chromosome);
        }

        public Member Breed(Member other)
        {
            float[] newChromosome = new float[Chromosome.Length];
            for (var i = 0; i < Chromosome.Length; i++)
                if (random.NextDouble() <= CROSSOVER_CHANCE)
                    newChromosome[i] = (0.65f * Chromosome[i] + 0.35f * other.Chromosome[i]);
            return new Member(newChromosome, _fitnessFunction);
        }
    }
}