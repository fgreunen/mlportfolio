using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace GeneticInvestor.Core
{
    public static class Extensions
    {
        public static float StdDev(this IEnumerable<float> values)
        {
            float ret = 0;
            int count = values.Count();
            if (count > 1)
            {
                //Compute the Average
                float avg = values.Average();

                //Perform the Sum of (value-avg)^2
                float sum = values.Sum(d => (d - avg) * (d - avg));

                //Put it all together
                ret = (float)Math.Sqrt(sum / count);
            }
            return ret;
        }
    }
}