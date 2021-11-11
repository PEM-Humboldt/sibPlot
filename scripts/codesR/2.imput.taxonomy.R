##########################################################################coreRDS.Forests
#dataForests <- readRDS("~/Dropbox/ForestsRDS/dataForests.rds")
#dataForests <- list()

################################################################################taxonomy
dataForests$taxonomy <- list()

setwd("~/Dropbox/ForestsRDS/core/taxonomy")
for(ifile in 1:length(dir())) {
	taxonomy.file <- as.data.frame(read.csv(dir()[ifile]))
	taxonomy.attributes <- c("code", 
							 "tag", 
							 "voucher", 
							 "catalogNumber", 
							 "family", 
							 "genus",
							 "specificEpithet", 
							 "infraspecificEpithet",
							 "digitalVoucher",
							 "digitalVoucher",
							 "recordedBy",
							 "eventDate", 
							 "identifiedBy", 
							 "identifiedDate", 
							 "comments")
	taxonomy.file.aux <- as.data.frame(matrix(NA, nrow=nrow(taxonomy.file), ncol=length(taxonomy.attributes), dimnames=list(c(), taxonomy.attributes)))	
	for (iname in 1:length(names(taxonomy.file))) {
		name <- names(taxonomy.file)[iname]
		if(substr(name, 1, 4)=="X...") {
		names(taxonomy.file)[iname] <- unlist(strsplit(name,"X..."))[2]
		}
	}            
	taxonomy.file.aux$"code" <- as.character(taxonomy.file$"code")
	taxonomy.file.aux$"tag" <- as.character(taxonomy.file$"tag")
	taxonomy.file.aux$"voucher" <- as.character(taxonomy.file$"voucher")
	taxonomy.file.aux$"catalogNumber" <- as.character(taxonomy.file$"catalogNumber")
	taxonomy.file.aux$"family" <- as.character(taxonomy.file$"family")
	taxonomy.file.aux$"genus" <- as.character(taxonomy.file$"genus")
	taxonomy.file.aux$"specificEpithet" <- as.character(taxonomy.file$"specificEpithet")
	taxonomy.file.aux$"infraspecificEpithet" <- as.character(taxonomy.file$"infraspecificEpithet")
	taxonomy.file.aux$"digitalVoucher" <- as.character(taxonomy.file$"digitalVoucher")	
	taxonomy.file.aux$"recordedBy" <- as.character(taxonomy.file$"recordedBy")
	if(any(!is.na(taxonomy.file$"eventDate"))) {
		aux.date <- as.character(taxonomy.file$"eventDate")
		aux.eventDate <- as.Date(rep(NA, length(aux.date)))
		for (idate in 1:length(aux.date)) {
			temp.date <- aux.date[idate]
			aux.months <- as.character(lubridate::month(1:12, label=TRUE, abbr = FALSE))
			aux2date <- strsplit(aux.date[idate], "/")[[1]]
			if (temp.date=="") {
				aux.eventDate[idate] <- as.Date(NA)
			} else if (any(aux.months %in% aux2date[1])==TRUE) {
				number.month <- which(aux.months %in% aux2date[1])
				aux.temp.date <- paste(number.month, aux2date[2], aux2date[3], sep="/")
				if (nchar(aux2date[3])==4) {aux.eventDate[idate] <- as.Date(aux.temp.date, "%m/%d/%Y")}
				if (nchar(aux2date[3])==2) {aux.eventDate[idate] <- as.Date(aux.temp.date, "%m/%d/%y")}
			} else if (as.numeric(aux2date[1]) >= 12 & as.numeric(aux2date[2]) <= 12 & as.numeric(aux2date[1])!=as.numeric(aux2date[2])) {
				if (nchar(aux2date[3])==4) {aux.eventDate[idate] <- as.Date(paste(aux2date[c(2,1,3)], collapse="/"), "%m/%d/%Y")}
				if (nchar(aux2date[3])==2) {aux.eventDate[idate] <- as.Date(paste(aux2date[c(2,1,3)], collapse="/"), "%m/%d/%y")}		
			} else {
				if (nchar(aux2date[3])==4) {aux.eventDate[idate] <- as.Date(paste(aux2date[c(1,2,3)], collapse="/"), "%m/%d/%Y")}
				if (nchar(aux2date[3])==2) {aux.eventDate[idate] <- as.Date(paste(aux2date[c(1,2,3)], collapse="/"), "%m/%d/%y")}
			}
			taxonomy.file.aux$"eventDate" <- aux.eventDate 
		}
	} else {
		taxonomy.file.aux$"eventDate" <- taxonomy.file$"eventDate"
	}	
	taxonomy.file.aux$"identifiedBy" <- as.character(taxonomy.file$"identifiedBy")
	if(any(!is.na(taxonomy.file$"identifiedDate"))) {
		aux.date <- as.character(taxonomy.file$"identifiedDate")
		aux.identifiedDate <- as.Date(rep(NA, length(aux.date)))
		for (idate in 1:length(aux.date)) {
			temp.date <- aux.date[idate]
			aux.months <- as.character(lubridate::month(1:12, label=TRUE, abbr = FALSE))
			aux2date <- strsplit(aux.date[idate], "/")[[1]]
			if (temp.date=="") {
				aux.identifiedDate[idate] <- as.Date(NA)
			} else if (any(aux.months %in% aux2date[1])==TRUE) {
				number.month <- which(aux.months %in% aux2date[1])
				aux.temp.date <- paste(number.month, aux2date[2], aux2date[3], sep="/")
				if (nchar(aux2date[3])==4) {aux.identifiedDate[idate] <- as.Date(aux.temp.date, "%m/%d/%Y")}
				if (nchar(aux2date[3])==2) {aux.identifiedDate[idate] <- as.Date(aux.temp.date, "%m/%d/%y")}
			} else if (as.numeric(aux2date[1]) >= 12 & as.numeric(aux2date[2]) <= 12 & as.numeric(aux2date[1])!=as.numeric(aux2date[2])) {
				if (nchar(aux2date[3])==4) {aux.identifiedDate[idate] <- as.Date(paste(aux2date[c(2,1,3)], collapse="/"), "%m/%d/%Y")}
				if (nchar(aux2date[3])==2) {aux.identifiedDate[idate] <- as.Date(paste(aux2date[c(2,1,3)], collapse="/"), "%m/%d/%y")}		
			} else {
				if (nchar(aux2date[3])==4) {aux.identifiedDate[idate] <- as.Date(paste(aux2date[c(1,2,3)], collapse="/"), "%m/%d/%Y")}
				if (nchar(aux2date[3])==2) {aux.identifiedDate[idate] <- as.Date(paste(aux2date[c(1,2,3)], collapse="/"), "%m/%d/%y")}
			}
			taxonomy.file.aux$"identifiedDate" <- aux.identifiedDate 
		}
	} else {
		taxonomy.file.aux$"identifiedDate" <- taxonomy.file$"identifiedDate"
	}	               		
	taxonomy.file.aux$"comments" <- as.character(taxonomy.file$"comments")
	final.file <- taxonomy.file.aux
	temp.ext <- strsplit(strsplit(dir()[ifile], "_")[[1]][2], ".csv")[[1]]
	dataForests$taxonomy[[ifile]] <- final.file
	names(dataForests$taxonomy)[ifile] <- temp.ext
}
######################################## DO NOT RUN #######################################
#saveRDS(dataForests, "~/Dropbox/ForestsRDS/dataForests.rds")
######################################## DO NOT RUN #######################################