using System;
using System.Linq;

namespace GeneticInvestor.Core
{
    public class Population
    {
        private Random _rnd = new Random();
        private int _chromosomeLength;
        private float _mutationRate;
        private float _mutationAmount;
        private Member[] _members;
        private bool _allowNegative;
        private int _rankSum;
        public Member BestMember
        {
            get
            {
                return _members.Last();
            }
        }

        public Population(
            float[][] chromosomes, Func<float[], float> fitnessFunction,
            float mutationRate = 0.025f,
            float mutationAmount = 0.5f,
            bool allowNegative = false)
        {
            _allowNegative = allowNegative;
            _chromosomeLength = chromosomes[0].Length;
            _mutationRate = mutationRate;
            _mutationAmount = Math.Abs(mutationAmount);
            _members = new Member[chromosomes.Length];

            for (int i = 0; i < _members.Length; i++)
                _members[i] = new Member(chromosomes[i], fitnessFunction);

            SortMembers();
        }

        public void Iterate()
        {
            Member[] newMembers = new Member[_members.Length];
            for (var i = 0; i < _members.Length; i++)
            {
                var parent1Index = RouletteWheelSelection();
                var parent2Index = RouletteWheelSelection();
                newMembers[i] = _members[parent1Index].Breed(_members[parent2Index]);
            }
            _members = newMembers;
            Mutate();
        }

        public float GetMaxFitness()
        {
            return _members.Last().Fitness();
        }

        public int RouletteWheelSelection()
        {
            int total = 0;
            int threshold = (int)(_rankSum * _rnd.NextDouble());
            for (var i = 0; i < _members.Length; i++)
            {
                total += _members[i].Rank;
                if (total >= threshold) return i;
            }
            return _members.Length - 1;
        }

        private void Mutate()
        {
            for (var i = 0; i < _members.Length; i++)
                for (var j = 0; j < _chromosomeLength; j++)
                    if (_rnd.NextDouble() <= _mutationRate)
                        _members[i].Chromosome[j] += (_rnd.NextDouble() < 0.5 ? 1 : (_allowNegative ? -1 : 1)) * (float)(_rnd.NextDouble() * _mutationAmount);
            SortMembers();
        }

        private void SortMembers()
        {
            _members = _members.OrderBy(x => x.Fitness()).ToArray();
            for (var i = 0; i < _members.Length; i++)
                _members[i].Rank = i;

            if (_rankSum == 0) _rankSum = _members.Sum(x => x.Rank);
        }
    }
}