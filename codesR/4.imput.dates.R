##########################################################################coreRDS.Forests
#dataForests <- readRDS("~/Dropbox/ForestsRDS/dataForests.rds")
#dataForests <- list()

################################################################################censuses
dataForests$dates <- list()

#################################################################################dates
setwd("~/Dropbox/ForestsRDS/core/dates")
for(ifile in 1:length(dir())) {
	dates.file <- read.csv(dir()[ifile])
	dates.attributes <- c("measuringType", "eventDate", "comments")
	dates.file.aux <- as.data.frame(matrix(NA, nrow=nrow(dates.file), ncol=length(dates.attributes), dimnames=list(c(), dates.attributes)))	
	for (iname in 1:length(names(dates.file))) {
		name <- names(dates.file)[iname]
		if(substr(name, 1, 4)=="X...") {
		names(dates.file)[iname] <- unlist(strsplit(name,"X..."))[2]
		}
	}
	dates.file.aux$"measuringType" <- as.character(dates.file$"measuringType")
	if(any(!is.na(dates.file$"eventDate"))) {
		aux.date <- as.character(dates.file$"eventDate")
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
			dates.file.aux$"eventDate" <- aux.eventDate 
		}
	} else {
		dates.file.aux$"eventDate" <- dates.file$"eventDate"
	}	               	
	dates.file.aux$"comments" <- as.character(dates.file$"comments")
	final.file <- dates.file.aux
	temp.ext <- strsplit(strsplit(dir()[ifile], "_")[[1]][2], ".csv")[[1]]
	dataForests$dates[[ifile]] <- final.file
	names(dataForests$dates)[ifile] <- temp.ext
}

######################################## DO NOT RUN #######################################
#saveRDS(dataForests, "~/Dropbox/ForestsRDS/dataForests.rds")
######################################## DO NOT RUN #######################################