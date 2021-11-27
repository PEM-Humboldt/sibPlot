# folderCensus1 <- "../../CodeRoy/BasesHumboldt-MV/core/census1/"
read_census1<-lapply(paste0(folderCensus1,dir(folderCensus1,pattern="^.*\\.csv$")),read.csv)
regexFile <- "^([a-z0-9]+)_([A-Za-z0-9]+)\\.csv"
grepl(regexFile,dir(folderCensus1,pattern="^.*\\.csv$"))
plotName <- sub(regexFile,"\\2",dir(folderCensus1,pattern="^.*\\.csv$"))
type <- sub(regexFile,"\\1",dir(folderCensus1,pattern="^.*\\.csv$"))
names(read_census1) <- plotName
listCensus1<-tapply(read_census1,type,function(x)x)
dataForests$censuses$census1<-listCensus1