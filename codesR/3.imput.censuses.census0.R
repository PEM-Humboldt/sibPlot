##########################################################################coreRDS.Forests
#dataForests <- readRDS("~/Dropbox/ForestsRDS/dataForests.rds")
#dataForests <- list()

################################################################################censuses
dataForests$censuses <- list()

#################################################################################census0
setwd("~/Dropbox/ForestsRDS/core/census0")
for(ifile in 1:length(dir())) {
	census0.file <- read.csv(dir()[ifile])
	census0.attributes <- c("tag", 
							"code",
							"subplot",
							"ramet",
							"DBH_cm",
							"POM_m",
							"height_m",
							"comments")
	census0.file.aux <- as.data.frame(matrix(NA, nrow=nrow(census0.file), ncol=length(census0.attributes), dimnames=list(c(), census0.attributes)))	
	for (iname in 1:length(names(census0.file))) {
		name <- names(census0.file)[iname]
		if(substr(name, 1, 4)=="X...") {
		names(census0.file)[iname] <- unlist(strsplit(name,"X..."))[2]
		}
	}
	census0.file.aux$"tag" <- as.character(census0.file$"tag")
	census0.file.aux$"code" <- as.character(census0.file$"code") 
	census0.file.aux$"subplot"  <- as.numeric(census0.file$"subplot")
	census0.file.aux$"ramet" <- as.integer(census0.file$"ramet")             
	census0.file.aux$"DBH_cm" <- as.numeric(census0.file$"DBH_cm")
	census0.file.aux$"POM_m" <- as.numeric(census0.file$"POM_m")
	if(any(names(census0.file)=="DAH_cm")==TRUE | any(names(census0.file)=="POM_DAH_m")==TRUE){
			census0.file.aux$"DAH_cm" <- as.numeric(census0.file$"DBH_cm")                   
			census0.file.aux$"POM_DAH_m" <- as.numeric(census0.file$"POM_m")
	} 
	census0.file.aux$"height_m" <- as.numeric(census0.file$"height_m")                  	
	census0.file.aux$"comments" <- as.character(census0.file$"comments")
	final.file <- census0.file.aux
	temp.ext <- strsplit(strsplit(dir()[ifile], "_")[[1]][2], ".csv")[[1]]
	dataForests$censuses$census0[[ifile]] <- final.file
	names(dataForests$censuses$census0)[ifile] <- temp.ext
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Taxonomy Valitadion.I
if(all(names(dataForests$censuses$census0) %in% names(dataForests$taxonomy))) {
	message("Congrats! all plots have taxonomy data")
} else {
	plotswhitoutTaxonomy <- names(dataForests$censuses$census0)[which(names(dataForests$censuses$census0) %in% names(dataForests$taxonomy) == FALSE)]
	warning(paste0("plots ``", paste(plotswhitoutTaxonomy, collapse=", "), "´´ do not have taxonomy!!!"))
}

if(all(names(dataForests$taxonomy) %in% names(dataForests$censuses$census0))) {
	message("Congrats! all plots have census0 data")
} else {
	plotswhitoutCensus0 <- names(dataForests$taxonomy)[which(names(dataForests$taxonomy) %in% names(dataForests$censuses$census0) == FALSE)]
	warning(paste0("plots ``", paste(plotswhitoutCensus0, collapse=", "), "´´ do not have census0 data!!!"))
}

for (iplot in 1:length(names(dataForests$censuses$census0))) {
	plotname <- names(dataForests$censuses$census0)[iplot]
	spsCensus0 <- dataForests$censuses$census0[[which(names(dataForests$censuses$census0)==plotname)]]$code
	spsTaxonomy <- unique(dataForests$taxonomy[[which(names(dataForests$taxonomy)==plotname)]]$code)
	if(all(spsCensus0 %in% spsTaxonomy)){
		message(paste0("All species in ``", plotname, "´´ census0 are reported in taxonomy of ``", plotname, "´´"))
	} else {
		spswithoutTaxonomy <- unique(spsCensus0[which(spsCensus0 %in% spsTaxonomy)==FALSE])
		stop(paste0("Species ``", paste(spswithoutTaxonomy, collapse=", "), "´´are not reported in taxonomy data of ``", plotname, "´´"))
	}
}

######################################## DO NOT RUN #######################################
#saveRDS(dataForests, "~/Dropbox/ForestsRDS/dataForests.rds")
######################################## DO NOT RUN #######################################