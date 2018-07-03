using System;

namespace GeneticInvestor.Core
{
    public class TrainResult
    {
        public DateTime Timestamp { get; set; }
        public float Returns { get; set; }
        public float MeanReturns { get; set; }
        public float MinReturns { get; set; }
        public float MaxReturns { get; set; }
        public float StdDevReturns { get; set; }

        public float PX_TO_SALES_RATIO { get; set; }
        public float S_PX_TO_SALES_RATIO { get; set; }
        public float CASH_FLOW_YIELD { get; set; }
        public float S_CASH_FLOW_YIELD { get; set; }
        public float EARN_YLD { get; set; }
        public float S_EARN_YLD { get; set; }
        public float EQY_DVD_YLD_12M { get; set; }
        public float S_EQY_DVD_YLD_12M { get; set; }
        public float PX_TO_BOOK_RATIO { get; set; }
        public float S_PX_TO_BOOK_RATIO { get; set; }
    }
}