using CsvHelper;
using GeneticInvestor.Core;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace GeneticInvestor.Console
{
    public class Program
    {
        public static void Main(string[] args)
        {
            const string outputPath = @"C:\Working\results.csv";
            var files = Directory.EnumerateFiles(@"C:\Working\Inputs_Returns").ToList();

            ConcurrentBag<TrainResult> results = new ConcurrentBag<TrainResult>();
            Parallel.For(0, files.Count, file =>
            {
                //for (int j = 55; j < 56; j++)
                //{
                var filename = files[file];
                var lines = File.ReadAllLines(filename);
                List<AllocationRun> allocationRuns = new List<AllocationRun>();
                for (int i = 1; i < lines.Length; i++)
                    if (!string.IsNullOrEmpty(lines[i]))
                        allocationRuns.Add(new AllocationRun(filename, lines[i].Trim().Split(",")));

                float[] returns = allocationRuns.Select(x => x.RETURNS).ToArray();
                float[][] values =
                {
                    allocationRuns.Select(x => x.PX_TO_SALES_RATIO).ToArray(),
                    allocationRuns.Select(x => x.CASH_FLOW_YIELD).ToArray(),
                    allocationRuns.Select(x => x.EARN_YLD).ToArray(),
                    allocationRuns.Select(x => x.EQY_DVD_YLD_12M).ToArray(),
                    allocationRuns.Select(x => x.PX_TO_BOOK_RATIO).ToArray()
                };

                Func<float[], float> returnsFunc = (chromosome) =>
                {
                    var valueWeights = new float[returns.Length];
                    for (int i = 0; i < returns.Length; i++)
                    {
                        float weight = 0;
                        for (var j = 0; j < values[i].Length; j++)
                            weight += chromosome[j] * values[i][j];
                        valueWeights[i] = weight;
                    }
                    var weightsTotal = valueWeights.Sum();

                    float fitnessValue = 0;
                    for (var i = 0; i < returns.Length; i++)
                        fitnessValue += returns[i] * (valueWeights[i] / weightsTotal);
                    return fitnessValue;
                };

                var members = Trainer.Train(allocationRuns, returns, values, returnsFunc, 250).ToList();
                var index = Math.Max((members.Count / 2) - 1, 0);
                var member = members[index];

                var meanReturn = allocationRuns.Select(x => x.RETURNS).Average();
                var maxReturn = allocationRuns.Select(x => x.RETURNS).Max();
                var minReturn = allocationRuns.Select(x => x.RETURNS).Min();
                var sdReturn = allocationRuns.Select(x => x.RETURNS).StdDev();
                var maxFitness = member.Fitness();
                var total = member.Chromosome.Sum();
                var bestReturns = returnsFunc(member.Chromosome);

                var trainResult = new TrainResult
                {
                    Timestamp = allocationRuns[0].Timestamp,
                    MaxReturns = maxReturn,
                    MinReturns = minReturn,
                    MeanReturns = meanReturn,
                    StdDevReturns = sdReturn,
                    Returns = bestReturns,
                    PX_TO_SALES_RATIO = member.Chromosome[0],
                    S_PX_TO_SALES_RATIO = member.Chromosome[0] / total,
                    CASH_FLOW_YIELD = member.Chromosome[1],
                    S_CASH_FLOW_YIELD = member.Chromosome[1] / total,
                    EARN_YLD = member.Chromosome[2],
                    S_EARN_YLD = member.Chromosome[2] / total,
                    EQY_DVD_YLD_12M = member.Chromosome[3],
                    S_EQY_DVD_YLD_12M = member.Chromosome[3] / total,
                    PX_TO_BOOK_RATIO = member.Chromosome[4],
                    S_PX_TO_BOOK_RATIO = member.Chromosome[4] / total
                };
                results.Add(trainResult);
                System.Console.WriteLine($"Progress: {results.Count / (float)files.Count}");
                //}
            });
            var listResults = results.ToList().OrderBy(x => x.Timestamp);

            if (File.Exists(outputPath)) File.Delete(outputPath);
            using (var writer = new StreamWriter(outputPath))
            {
                var csv = new CsvWriter(writer);
                csv.WriteRecords(listResults);
            }

            System.Console.WriteLine("DONE");
            System.Console.ReadKey();
        }
    }
}