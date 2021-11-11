###################################Don't forget get, change or set - Working Directory#
dir_data<-"~/Dropbox/DataSets/TDF_IAvH/"

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::taxonomyData
read.taxonomy=function(site,pplot)
{
	path<-paste(dir_data, "taxonomy/taxonomy_", sep="")
	m<-paste(path,site,".csv",sep="")
	temp<-read.csv(m,sep=",",dec=".",h=T)
	
	data<-temp[,c("family",	"genus", "specificEpithet", "associatedMedia", "catalogNumber")]
	data$family <- as.character(data$family)
	data$genus <- as.character(data$genus)
	data$specificEpithet <- as.character(data$specificEpithet)
	data$associatedMedia <- as.character(data$associatedMedia)	
	data$catalogNumber <-	as.character(data$catalogNumber)
	return(data)
}
 
mplotnames<-sapply(strsplit(list.files(paste(dir_data, "taxonomy", sep="")), "taxonomy_|.csv"), "[", 2)
mplotnames<-na.exclude(mplotnames)
 
mparcnames<-mplotnames[1:length(mplotnames)]

library(gtools)####CHECK gtools library
 
for (i in 1:length(mplotnames))
{
 	temp=read.taxonomy(mplotnames[i],mparcnames[i])
 	if (i==1) taxonomy=temp
 	if (i>1)  taxonomy=smartbind(taxonomy,temp, fill=NA)
}

rownames(taxonomy)<-1:length(rownames(taxonomy))
head(taxonomy);dim(taxonomy)


searchSpecies <- "Trichilia"
url.taxonomy <- taxonomy[which(taxonomy$genus==searchSpecies), "associatedMedia"]
fmb.code <- taxonomy[which(taxonomy$genus==searchSpecies), "catalogNumber"]

url.taxonomy <- url.taxonomy[which(url.taxonomy!="")]
#fmb.code <- fmb.code[which(url.taxonomy!="")]


for (isps in 1:length(url.taxonomy)) {
	temp.url <- url.taxonomy[isps]
	download.file(temp.url, paste("~/Dropbox/dataForests/outputs/Output_Reports/", paste(searchSpecies, "ejemplar", isps, ".jpg", sep="_"), sep=""))
}
