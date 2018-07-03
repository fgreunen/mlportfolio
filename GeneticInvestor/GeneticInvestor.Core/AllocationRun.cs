using System;
using System.IO;

namespace GeneticInvestor.Core
{
    public class AllocationRun
    {
        public AllocationRun(string filename, string[] splitted)
        {
            RETURNS = float.Parse(splitted[0]);
            PX_TO_SALES_RATIO = float.Parse(splitted[1]);
            CASH_FLOW_YIELD = float.Parse(splitted[2]);
            EARN_YLD = float.Parse(splitted[3]);
            EQY_DVD_YLD_12M = float.Parse(splitted[4]);
            PX_TO_BOOK_RATIO = float.Parse(splitted[5]);

            // THIS IS A POTENTIAL ISSUE!!!
            PX_TO_SALES_RATIO = Math.Max(PX_TO_SALES_RATIO, 0);
            CASH_FLOW_YIELD = Math.Max(CASH_FLOW_YIELD, 0);
            EARN_YLD = Math.Max(EARN_YLD, 0);
            EQY_DVD_YLD_12M = Math.Max(EQY_DVD_YLD_12M, 0);
            PX_TO_BOOK_RATIO = Math.Max(PX_TO_BOOK_RATIO, 0);

            Timestamp =
                DateTime.Parse(Path.GetFileNameWithoutExtension(filename).Substring(7));
        }

        public DateTime Timestamp { get; set; }
        public float RETURNS { get; set; }
        public float PX_TO_SALES_RATIO { get; set; }
        public float CASH_FLOW_YIELD { get; set; }
        public float EARN_YLD { get; set; }
        public float EQY_DVD_YLD_12M { get; set; }
        public float PX_TO_BOOK_RATIO { get; set; }
    }
}