mongo_DatabaseName = "GeneticInvestor"
mongo_HostName = "localhost"
mongo_Port = 27017

mongo.delete = function(collectionName) {
    mongo = mongoDbConnect(mongo_DatabaseName, mongo_HostName, mongo_Port)
    output = dbRemoveQuery(mongo, collectionName, "{}")
    dbDisconnect(mongo)
}
mongo.create = function(collectionName, document) {
    mongo = mongoDbConnect(mongo_DatabaseName, mongo_HostName, mongo_Port)
    output = dbInsertDocument(mongo, collectionName, document)
    dbDisconnect(mongo)
}
mongo.get = function(collectionName) {
    mongo = mongoDbConnect(mongo_DatabaseName, mongo_HostName, mongo_Port)
    output = dbGetQuery(mongo, collectionName, "{}", 0, 10000000)
    dbDisconnect(mongo)
    return(output)
}