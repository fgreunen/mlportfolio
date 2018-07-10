using MongoDB.Driver;
using System.Collections.Generic;

namespace GeneticInvestor.Core
{
    public static class Mongo
    {
        const string CONNECTION = "mongodb://localhost:27017";
        const string DATABASE = "GeneticInvestor";

        public static List<Input> GetInputs()
        {
            var client = new MongoClient(CONNECTION);
            var database = client.GetDatabase(DATABASE);
            var collection = database.GetCollection<Input>("Inputs").Find(_ => true).ToList();
            return collection;
        }

        public static void SetOutputs(List<Output> outputs)
        {
            var client = new MongoClient(CONNECTION);
            var database = client.GetDatabase(DATABASE);
            database.GetCollection<Output>("Outputs").DeleteMany(_ => true);
            database.GetCollection<Output>("Outputs").InsertMany(outputs);
        }
    }
}
