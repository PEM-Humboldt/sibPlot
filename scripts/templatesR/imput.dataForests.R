##########################################################################coreRDS.Forests
dataForests <- list()

################################################################################metadata
dataForests$metadata <- list()
setwd("~/Dropbox/MonitoreoBosques/metadata")
for(ifile in 1:length(dir())) {
	temp.file <- as.data.frame(t(read.csv(dir()[ifile], row.names=1)))	
	aux.date <- strsplit(as.character(temp.file$"register_date"), "/")[[1]]
	if (aux.date[2]<=12) {
		temp.file$"register_date" <- as.Date(paste(aux.date[c(2,1,3)], collapse="/"), "%m/%d/%y") 
	}
	if (aux.date[2]>12) {
		temp.file$"register_date" <- as.Date(temp.file$"register_date", "%m/%d/%y") 
	}
	temp.file$"latitude_dec" <- as.numeric(as.character(temp.file$"latitude_dec"))                
	temp.file$"longitude_dec" <- as.numeric(as.character(temp.file$"longitude_dec"))                
	temp.file$"altitude_m"  <- as.numeric(as.character(temp.file$"altitude_m"))                  
	temp.file$"area_ha" <- as.numeric(as.character(temp.file$"area_ha"))                      
	temp.file$"area_type" <- as.character(temp.file$"area_type")                   
	temp.file$"plot_type" <- as.character(temp.file$"plot_type")                    
	temp.file$"shape_type" <- as.character(temp.file$"shape_type")                  
	temp.file$"length_m" <- as.numeric(as.character(temp.file$"length_m"))                     
	temp.file$"width_m" <- as.numeric(as.character(temp.file$"width_m"))                     
	temp.file$"subplot_area_ha" <- as.numeric(as.character(temp.file$"subplot_area_ha"))              
	temp.file$"region" <- as.character(temp.file$"region")                      
	temp.file$"ecosystem"  <- as.character(temp.file$"ecosystem")                    
	temp.file$"location" <- as.character(temp.file$"location")                    
	temp.file$"state" <- as.character(temp.file$"state")                       
	temp.file$"country" <- as.character(temp.file$"country")                     
	temp.file$"province" <- as.character(temp.file$"province")                     
	temp.file$"terrain_type" <- as.character(temp.file$"terrain_type")                
	temp.file$"forest_composition" <- as.character(temp.file$"forest_composition")           
	temp.file$"substrate_geology" <- as.character(temp.file$"substrate_geology")           
	temp.file$"forest_status" <- as.character(temp.file$"forest_status")                
	temp.file$"general_slope" <- as.character(temp.file$"general_slope")               
	temp.file$"nearest_anthropogenic_edge_m" <- as.numeric(as.character(temp.file$"nearest_anthropogenic_edge_m")) 
	temp.file$"fragment_size" <- as.numeric(as.character(temp.file$"fragment_size"))               
	temp.file$"comments" <- as.character(temp.file$"comments")
	final.file <- temp.file
	temp.ext <- strsplit(strsplit(dir()[ifile], "_")[[1]][2], ".csv")[[1]]
	dataForests$metadata[[ifile]] <- final.file
	names(dataForests$metadata)[ifile] <- temp.ext
}

###################################################################################dates
dataForests$dates <- list()

#################################################################################members
dataForests$members <- list()

################################################################################taxonomy
dataForests$taxonomy <- list()

################################################################################censuses
dataForests$censuses <- list()
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::patial.censuses
setwd("~/Dropbox/MonitoreoBosques/census0")
for(ifile in 1:length(dir())) {
	temp.file <- read.csv(dir()[ifile])
	temp.file$"tag" <- as.character(temp.file$"tag")                
	temp.file$"code" <- as.character(temp.file$"code")                
	temp.file$"subplot"  <- as.numeric(temp.file$"subplot")                 
	temp.file$"ramet" <- as.integer(temp.file$"ramet")                 
	temp.file$"DBH_cm" <- as.numeric(temp.file$"DBH_cm")                   
	temp.file$"POM_m" <- as.numeric(temp.file$"POM_m") 
	if(any(names(temp.file)=="DAH_cm")==TRUE | any(names(temp.file)=="POM_DAH_m")==TRUE){
			temp.file$"DAH_cm" <- as.numeric(temp.file$"DBH_cm")                   
			temp.file$"POM_DAH_m" <- as.numeric(temp.file$"POM_m")
	}            
	temp.file$"height_m" <- as.numeric(temp.file$"height_m")                  
	temp.file$"comments" <- as.character(temp.file$"comments")                  
	final.file <- temp.file
	temp.ext <- strsplit(strsplit(dir()[ifile], "_")[[1]][2], ".csv")[[1]]
	dataForests$censuses$census0[[ifile]] <- final.file
	names(dataForests$censuses$census0)[ifile] <- temp.ext
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::patial.censuses
setwd("~/Dropbox/MonitoreoBosques/census1")
final.file <- list()
for(ifile in 1:length(dir())) {
	temp.type <- sapply(strsplit(dir()[ifile], "_"), "[", 1)
	if(temp.type=="growth1") {
		temp.file <- read.csv(dir()[ifile])
		temp.file$"tag" <- as.character(temp.file$"tag")
		temp.file$"DBH_cm" <- as.numeric(temp.file$"DBH_cm")
		temp.file <- read.csv(dir()[ifile])
		temp.file$"tag" <- as.character(temp.file$"tag")               
		temp.file$"DBH_cm" <- as.numeric(temp.file$"DBH_cm")                   
		if(any(names(temp.file)=="POM_m")==TRUE) {
			temp.file$"POM_m" <- as.numeric(temp.file$"POM_m") 
		} else {
			temp.file$"POM_m" <- rep(NA, nrow(temp.file))
		}
		if(any(names(temp.file)=="DAH_cm")==TRUE | any(names(temp.file)=="POM_DAH_m")==TRUE){
			temp.file$"DAH_cm" <- as.numeric(temp.file$"DBH_cm")                   
			temp.file$"POM_DAH_m" <- as.numeric(temp.file$"POM_m")
		}                  
		if(any(names(temp.file)=="height_m")==TRUE) {
			temp.file$"height_m" <- as.numeric(temp.file$"height_m") 
		}          
		temp.file$"comments" <- as.character(temp.file$"comments")
		growth1 <- temp.file
	}
	final.file$growth1 <- growth1
	if(temp.type=="mortality1") {
		temp.file <- read.csv(dir()[ifile])
		temp.file$"tag" <- as.character(temp.file$"tag")
		temp.file$"comments" <- as.character(temp.file$"comments")
		mortality1 <- temp.file
	}
	final.file$mortality1 <- mortality1
	if(temp.type=="recruitment1") {
		temp.file <- read.csv(dir()[ifile])
		temp.file$"tag" <- as.character(temp.file$"tag")                
		temp.file$"code" <- as.character(temp.file$"code")                
		temp.file$"subplot"  <- as.numeric(temp.file$"subplot")                 
		temp.file$"ramet" <- as.integer(temp.file$"ramet")                 
		temp.file$"DBH_cm" <- as.numeric(temp.file$"DBH_cm")                   
		if(any(names(temp.file)=="POM_m")==TRUE) {
			temp.file$"POM_m" <- as.numeric(temp.file$"POM_m") 
		} else {
			temp.file$"POM_m" <- rep(NA, nrow(temp.file))
		} 
		if(any(names(temp.file)=="DAH_cm")==TRUE | any(names(temp.file)=="POM_DAH_m")==TRUE){
			temp.file$"DAH_cm" <- as.numeric(temp.file$"DBH_cm")                   
			temp.file$"POM_DAH_m" <- as.numeric(temp.file$"POM_m")
		}            
		temp.file$"height_m" <- as.numeric(temp.file$"height_m")                  
		temp.file$"comments" <- as.character(temp.file$"comments") 
		recruitment1 <- temp.file	
	}    
	final.file$recruitment1 <- recruitment1
	#final.file[[2]]<- mortality1
	#final.file[[3]] <- recruitment1              
	#names(final.file) <- c("growth1", "mortality1", "recruitment1")
	#final.file <- temp.file
	temp.ext <- strsplit(strsplit(dir()[ifile], "_")[[1]][2], ".csv")[[1]]
	dataForests$censuses$census1[[ifile]] <- final.file
	names(dataForests$censuses$census1)[ifile] <- temp.ext
}


