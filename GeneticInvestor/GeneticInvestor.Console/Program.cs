using GeneticInvestor.Core;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace GeneticInvestor.Console
{
    public class Program
    {
        public static double[] GetFreeFloater(int length, int index)
        {
            double[] freeFloatings = new double[length];
            for (int i = 0; i < freeFloatings.Length; i++) freeFloatings[i] = index == i ? 1 : 0;
            return freeFloatings;
        }

        public static void Main(string[] args)
        {
            try
            {
                const double REGULARIZATION_COEFFICIENT = 0.000025;
                var inputs = Mongo.GetInputs();
                var outputs = new ConcurrentBag<Output>();
                foreach (var input in inputs)
                {
                    double[] returns = input.VALUES.TARGET.ToArray();
                    double[][] values =
                    {
                        input.VALUES.EARN_YLD.ToArray(),
                        input.VALUES.EQY_DVD_YLD_12M.ToArray(),
                        input.VALUES.PX_TO_SALES_RATIO.ToArray(),
                        input.VALUES.CASH_FLOW_YIELD.ToArray(),
                        input.VALUES.PX_TO_BOOK_RATIO.ToArray(),
                        //input.VALUES.OPER_INC_TO_NET_SALES.ToArray(),
                        //input.VALUES.MOMENTUM_1_MONTH.ToArray(),
                        //input.VALUES.MOMENTUM_2_WEEKS.ToArray(),
                        //input.VALUES.SALES_3YR_AVG_GROWTH.ToArray(),
                        //input.VALUES.MOMENTUM_6_MONTH.ToArray(),
                        //GetFreeFloater(returns.Length, 0),
                        //GetFreeFloater(returns.Length, 1),
                        //GetFreeFloater(returns.Length, 2),
                        //GetFreeFloater(returns.Length, 3),
                    };

                    Func<double[], double> targetFunc = (chromosome) =>
                    {
                        var valueWeights = new double[returns.Length];
                        for (int i = 0; i < returns.Length; i++)
                        {
                            double weight = 0;
                            for (var j = 0; j < chromosome.Length; j++)
                                weight += chromosome[j] * values[j][i];
                            valueWeights[i] = weight;
                        }
                        var weightsTotal = valueWeights.Sum();

                        double fitnessValue = 0;
                        for (var i = 0; i < returns.Length; i++)
                            fitnessValue += returns[i] * (valueWeights[i] / weightsTotal);
                        return (float)fitnessValue;
                    };

                    Func<double[], double> fitnessFunc = (chromosome) =>
                    {
                        var valueWeights = new double[returns.Length];
                        for (int i = 0; i < returns.Length; i++)
                        {
                            double weight = 0;
                            for (var j = 0; j < chromosome.Length; j++)
                                weight += chromosome[j] * values[j][i];
                            valueWeights[i] = weight;
                        }
                        var weightsTotal = valueWeights.Sum();

                        double fitnessValue = 0;
                        for (var i = 0; i < returns.Length; i++)
                            fitnessValue += returns[i] * (valueWeights[i] / weightsTotal);

                        double penalty = REGULARIZATION_COEFFICIENT * chromosome.Sum();
                        return (float)(fitnessValue - penalty);
                    };

                    //var list = new List<double>();
                    //for (int i = 0; i < 1; i++)
                    //{
                        var member = Trainer.Train(returns, values, fitnessFunc, 750);
                        var total = member.Chromosome.Sum();
                        var fitness = targetFunc(member.Chromosome);
                        var output = new Output
                        {
                            ACTUAL_TARGET = fitness,
                            MEAN_TARGET = input.VALUES.TARGET.Average(),
                            T_START = input.T_START,
                            T_END = input.T_END,
                            T_USEFROM = input.T_USEFROM,
                            EARN_YLD = member.Chromosome[0] / total,
                            EQY_DVD_YLD_12M = member.Chromosome[1] / total,
                            PX_TO_SALES_RATIO = member.Chromosome[2] / total,
                            CASH_FLOW_YIELD = member.Chromosome[3] / total,
                            PX_TO_BOOK_RATIO = member.Chromosome[4] / total,
                            //OPER_INC_TO_NET_SALES = member.Chromosome[5] / total,
                            //MOMENTUM_1_MONTH = member.Chromosome[6] / total,
                            //MOMENTUM_2_WEEKS = member.Chromosome[7] / total,
                            //SALES_3YR_AVG_GROWTH = member.Chromosome[8] / total,
                            //MOMENTUM_6_MONTH = member.Chromosome[9] / total,
                            //FREE_FLOATER_1 = member.Chromosome[10] / total,
                            //FREE_FLOATER_2 = member.Chromosome[11] / total,
                            //FREE_FLOATER_3 = member.Chromosome[12] / total,
                            //FREE_FLOATER_4 = member.Chromosome[13] / total,
                        };
                    //    list.Add(member.Chromosome[0] / total);
                    //    System.Console.WriteLine($"{member.Chromosome[0] / total} | {fitness}");
                    //}
                    //System.Console.WriteLine();
                    //System.Console.WriteLine(list.Average());
                    //System.Console.WriteLine(list.StdDev());

                    outputs.Add(output);
                    System.Console.WriteLine($"Progress: {outputs.Count / (float)inputs.Count}");
                }

                Mongo.SetOutputs(outputs.OrderBy(x => x.T_START).ToList());
            }
            catch (Exception ex)
            {
                System.Console.WriteLine(ex);
            }

            System.Console.WriteLine("DONE");

            System.Console.ReadKey();
        }
    }
}