using MongoDB.Bson;

namespace GeneticInvestor.Core
{
    public class Input
    {
        public class ValuesList
        {
            public double[] PX_TO_BOOK_RATIO { get; set; }
            public double[] EQY_DVD_YLD_12M { get; set; }
            public double[] EARN_YLD { get; set; }
            public double[] CASH_FLOW_YIELD { get; set; }
            public double[] PX_TO_SALES_RATIO { get; set; }
            public double[] OPER_INC_TO_NET_SALES { get; set; }
            public double[] MOMENTUM_1_MONTH { get; set; }
            public double[] MOMENTUM_2_WEEKS { get; set; }
            public double[] SALES_3YR_AVG_GROWTH { get; set; }
            public double[] MOMENTUM_6_MONTH { get; set; }
            public double[] TARGET { get; set; }
        }

        public ObjectId Id { get; set; }
        public string T_START { get; set; }
        public string T_END { get; set; }
        public string T_USEFROM { get; set; }
        public ValuesList VALUES { get; set; }
    }
}