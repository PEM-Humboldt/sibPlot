##########################################################################coreRDS.Forests
#dataForests <- readRDS("~/Dropbox/ForestsRDS/dataForests.rds")
dataForests <- list()

################################################################################metadata
dataForests$metadata <- list()
#setwd("~/Dropbox/ForestsRDS/core/metadata")
for(ifile in 1:length(dir(folderMetadata))) {
	metadata.file <- as.data.frame(t(read.csv(paste0(folderMetadata,dir(folderMetadata))[ifile], row.names=1)))	
	metadata.attributes <- c("registerDate",
							 "latitude_dec",
							 "longitude_dec",
							 "altitude_m",
							 "area_ha",
							 "areaType",
							 "plotType",
							 "shapeType",
							 "length_m",
							 "width_m",
							 "subplotArea_ha",
							 "region",
							 "ecosystem",
							 "locationName",
							 "locationType",
							 "state",
							 "country",
							 "province",
							 "terrainType",
							 "substrateGeology",
							 "generalSlope_deg",
							 "forestComposition",
							 "forestStatus",
							 "forestAge_yrs",
							 "nearestAnthropogenicEdge_m",
							 "fragmentSize_ha",
							 "comments")
	metadata.file.aux <- as.data.frame(matrix(NA, nrow=nrow(metadata.file), ncol=length(metadata.attributes), dimnames=list(c(), metadata.attributes)))
	if(!is.na(metadata.file$"registerDate")) {
		temp.date <- as.character(metadata.file$"registerDate")
		aux.months <- as.character(lubridate::month(1:12, label=TRUE, abbr = FALSE))
		aux.date <- strsplit(temp.date, "/")[[1]]	
		if (temp.date=="") {
			final.date <- as.Date(NA)
		} else if (any(aux.months %in% aux.date[1])==TRUE) {
			number.month <- which(aux.months %in% aux.date[1])
			aux.temp.date <- paste(number.month, aux.date[2], aux.date[3], sep="/")
			if (nchar(aux.date[3])==4) {final.date <- as.Date(aux.temp.date, "%m/%d/%Y")}
			if (nchar(aux.date[3])==2) {final.date <- as.Date(aux.temp.date, "%m/%d/%y")}
		} else if (as.numeric(aux.date[1]) >= 12 & as.numeric(aux.date[2]) <= 12 & as.numeric(aux.date[1])!=as.numeric(aux.date[2])) {
			if (nchar(aux.date[3])==4) {final.date <- as.Date(paste(aux.date[c(2,1,3)], collapse="/"), "%m/%d/%Y")}
			if (nchar(aux.date[3])==2) {final.date <- as.Date(paste(aux.date[c(2,1,3)], collapse="/"), "%m/%d/%y")}
		} else {
			if (nchar(aux.date[3])==4) {final.date <- as.Date(paste(aux.date[c(1,2,3)], collapse="/"), "%m/%d/%Y")}
			if (nchar(aux.date[3])==2) {final.date <- as.Date(paste(aux.date[c(1,2,3)], collapse="/"), "%m/%d/%y")}
		}
		metadata.file.aux$"registerDate" <- final.date 
	} else {
		metadata.file.aux$"registerDate" <- metadata.file$"registerDate"
	}
	metadata.file.aux$"latitude_dec" <- as.numeric(as.character(metadata.file$"latitude_dec"))                
	metadata.file.aux$"longitude_dec" <- as.numeric(as.character(metadata.file$"longitude_dec"))                
	metadata.file.aux$"altitude_m"  <- as.numeric(as.character(metadata.file$"altitude_m"))                  
	metadata.file.aux$"area_ha" <- as.numeric(as.character(metadata.file$"area_ha"))                      
	metadata.file.aux$"areaType" <- as.character(metadata.file$"areaType")                   
	metadata.file.aux$"plotType" <- as.character(metadata.file$"plotType")                    
	metadata.file.aux$"shapeType" <- as.character(metadata.file$"shapeType")                  
	metadata.file.aux$"length_m" <- as.numeric(as.character(metadata.file$"length_m"))                     
	metadata.file.aux$"width_m" <- as.numeric(as.character(metadata.file$"width_m"))                     
	metadata.file.aux$"subplotArea_ha" <- as.numeric(as.character(metadata.file$"subplotArea_ha"))              
	metadata.file.aux$"region" <- as.character(metadata.file$"region")                      
	metadata.file.aux$"ecosystem"  <- as.character(metadata.file$"ecosystem")                    
	metadata.file.aux$"locationName" <- as.character(metadata.file$"locationName")               
	metadata.file.aux$"locationType" <- as.character(metadata.file$"locationType")	
	metadata.file.aux$"state" <- as.character(metadata.file$"state")                       
	metadata.file.aux$"country" <- as.character(metadata.file$"country")                     
	metadata.file.aux$"province" <- as.character(metadata.file$"province")                    
	metadata.file.aux$"terrainType" <- as.character(metadata.file$"terrainType")                
	metadata.file.aux$"forestComposition" <- as.character(metadata.file$"forestComposition")           
	metadata.file.aux$"substrateGeology" <- as.character(metadata.file$"substrateGeology")           
	metadata.file.aux$"forestStatus" <- as.character(metadata.file$"forestStatus")                
	metadata.file.aux$"generalSlope_deg" <- as.character(metadata.file$"generalSlope_deg")               
	metadata.file.aux$"nearestAnthropogenicEdge_m" <- as.numeric(as.character(metadata.file$"nearestAnthropogenicEdge_m")) 
	metadata.file.aux$"fragmentSize_ha" <- as.numeric(as.character(metadata.file$"fragmentSize_ha"))               
	metadata.file.aux$"forestAge_yrs" <- as.character(metadata.file$"forestAge_yrs") 
	metadata.file.aux$"comments" <- as.character(metadata.file$"comments")
	final.metaData <- metadata.file.aux
	temp.ext <- strsplit(strsplit(dir(folderMetadata)[ifile], "_")[[1]][2], ".csv")[[1]]
	rownames(final.metaData) <- temp.ext
	dataForests$metadata[[ifile]] <- final.metaData
	names(dataForests$metadata)[ifile] <- temp.ext
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Plot Name Valitadion.I
metadata <- c()
for (iplot in 1:length(dataForests$metadata)) {
	metadata[iplot] <- rownames(dataForests$metadata[[iplot]])
}

for (iplot in 1:length(dataForests$metadata)) {
	core <- sort(sapply(strsplit(sapply(strsplit(dir(folderMetadata), "_"), "[", 2), "\\."), "[", 1))
	icore <- core[iplot]
	
	metadata <- sort(metadata)
	imetadata <- metadata[iplot]
	
	if(icore!=imetadata) {
		warning(paste0("plot names errors ``core = ", icore, ", metadata = ", imetadata,"´´"))
	} else {
		message(paste0("metadata ``", icore, "´´ was correctly upload"))
	}
}
######################################## DO NOT RUN #######################################
#saveRDS(dataForests, "~/Dropbox/ForestsRDS/dataForests.rds")
######################################## DO NOT RUN #######################################