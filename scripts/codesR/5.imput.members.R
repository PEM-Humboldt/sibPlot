##########################################################################coreRDS.Forests
#dataForests <- readRDS("~/Dropbox/ForestsRDS/dataForests.rds")
#dataForests <- list()

################################################################################censuses
dataForests$members <- list()

#################################################################################dates
#setwd("~/Dropbox/ForestsRDS/core/members")
for(ifile in 1:length(dir(folderMembers))) {
	members.file <- read.csv(paste0(folderMembers,dir(folderMembers))[ifile])
	members.attributes <- c("firstName", 
						    "middleName", 
						    "lastName",
						    "institution",
						    "email",
						    "sponsor",
						    "sampling",
						    "fieldRole",
						    "comments")
	members.file.aux <- as.data.frame(matrix(NA, nrow=nrow(members.file), ncol=length(members.attributes), dimnames=list(c(), members.attributes)))	
	for (iname in 1:length(names(members.file))) {
		name <- names(members.file)[iname]
		if(substr(name, 1, 4)=="X...") {
		names(members.file)[iname] <- unlist(strsplit(name,"X..."))[2]
		}
	}
	members.file.aux$"firstName" <- as.character(members.file$"firstName")
	members.file.aux$"middleName" <- as.character(members.file$"middleName")
	members.file.aux$"lastName" <- as.character(members.file$"lastName")
	members.file.aux$"institution" <- as.character(members.file$"institution")
	members.file.aux$"email" <- as.character(members.file$"email")
	members.file.aux$"sponsor" <- as.character(members.file$"sponsor")	
	members.file.aux$"sampling" <- as.character(members.file$"sampling")
	members.file.aux$"fieldRole" <- as.character(members.file$"fieldRole")
	members.file.aux$"comments" <- as.character(members.file$"comments")	
	final.file <- members.file.aux
	temp.ext <- strsplit(strsplit(dir(folderMembers)[ifile], "_")[[1]][2], ".csv")[[1]]
	dataForests$members[[ifile]] <- final.file
	names(dataForests$members)[ifile] <- temp.ext
}

######################################## DO NOT RUN #######################################
#saveRDS(dataForests, "~/Dropbox/ForestsRDS/dataForests.rds")
######################################## DO NOT RUN #######################################