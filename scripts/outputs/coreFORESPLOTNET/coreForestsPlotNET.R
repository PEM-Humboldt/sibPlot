##########################################################################dataPreparation
GDB <- readRDS("~/Dropbox/Roy_Tartu/Data/dataTDF.rds")

################################################Tree-byTree_Inventory_data in (J.Aguirre)
data.colnames <- c("PlotCode",
	"Plot Name",
	"Census date",
	"Tag number",
	"Family",
	"Species",
	"D",
	"POM",
	"Flag1",
	"Flag2",
	"Flag3",
	"Flag4",
	"How was plant identified?",
	"",
	"voucher code","voucher collected",
	"TreeID (ForestPlots)",
	"Height",
	"Height Broken At",
	"Flag5",
	"original identification",
	"Stem tag grouping",
	"SubPlot",
	"X",
	"Y",
	"Tree Notes",
	"Extra D",
	"Extra POM",
	"DPOMtminus1",
	"LI",
	"CI",
	"CF",
	"CD1",
	"CD2",
	"Census Notes")
################################################################################ Structure
structure <- GDB$structure 
data.structure <- as.data.frame(matrix(NA, nrow=nrow(structure), ncol=length(data.colnames)))
colnames(data.structure) <- data.colnames

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Plot Name
data.structure$"Plot Name" <- GDB$structure$plot
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Census date
first.census <- subset(GDB$dates, measuring_type=="structure")
censusDate <- first.census$measuring_date[match(data.structure$"Plot Name",first.census$plot)]
data.structure$"Census date" <- censusDate
data.structure$"Tag number" <- GDB$structure$ind

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Family / Species
plot.code.str <- paste(structure$plot,structure$code, sep=".")
plot.code.tax <- paste(GDB$taxonomy$plot,GDB$taxonomy$code, sep=".")

family <- droplevels(GDB$taxonomy$family[match(plot.code.str,plot.code.tax)])
genus <- droplevels(GDB$taxonomy$genus[match(plot.code.str,plot.code.tax)])
sp_epithet <- droplevels(GDB$taxonomy$sp_epithet[match(plot.code.str,plot.code.tax)])

data.structure$"Family" <- family
data.structure$"Species" <- paste(genus, sp_epithet, sep=" ")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::D, POM
data.structure$"D" <- round(structure$DBH_cm_ini*10)
for (istem in 1:nrow(structure)) {
	if (!is.na(structure$DBH_cm_ini[istem])) {data.structure$"POM"[istem] <- 130}
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Flags1-4
data.structure$"Flag1" <- "a"
data.structure$"Flag2" <- 1
for (istem in 1:nrow(data.structure)) {
	if (!is.na(data.structure$"D"[istem])) {
		data.structure$"Flag3"[istem] <- 0
		data.structure$"Flag4"[istem] <- 0
	}
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: Vouchered
voucherData <- GDB$taxonomy[which(GDB$taxonomy$collected.code!=""),]
voucherData <- voucherData[which(!duplicated(paste(voucherData$plot, voucherData$code, sep="."))),]
voucherData$ind <-  as.character(voucherData$ind)
voucherData[is.na(voucherData$ind),]$ind <- "referred"
voucherData[which(voucherData$ind==""),]$ind <- "referred"

plot.code.ind.str <- paste(structure$plot, structure$code, structure$ind, sep=".")
plot.code.ind.vou <- paste(voucherData$plot, voucherData$code, voucherData$ind, sep=".")

data.structure$"voucher code" <- voucherData$collected.code[match(plot.code.ind.str, plot.code.ind.vou)]
data.structure$"voucher collected"[which(!is.na(data.structure$"voucher code"))] <- 1
data.structure$"How was plant identified?"[which(!is.na(data.structure$"voucher code"))] <- "Collected"

voucherReffered <- voucherData[which(voucherData$ind=="referred"),]

plot.code.str <- paste(structure$plot, structure$code, sep=".")
plot.code.vouref <- paste(voucherReffered$plot, voucherReffered$code, sep=".")

data.structure$"voucher code"[which(!is.na(match(plot.code.str, plot.code.vouref)))] <- voucherReffered$collected.code[match(plot.code.str, plot.code.vouref)][which(!is.na(voucherReffered$collected.code[match(plot.code.str, plot.code.vouref)]))] 

data.structure$"voucher code"[!is.na(data.structure$"voucher code")][duplicated(data.structure$"voucher code"[!is.na(data.structure$"voucher code")])] <- NA

for (istem in 1:nrow(data.structure)) {
	if (is.na(data.structure$"voucher collected"[istem]) & !is.na(data.structure$"voucher code"[istem])) {
		data.structure$"voucher collected"[istem] <- 0
	}
}

for (istem in 1:nrow(data.structure)) {
	if (!is.na(data.structure$"voucher collected"[istem]) & data.structure$"voucher collected"[istem]==0) {
		data.structure$"How was plant identified?"[istem] <- "Collected"
	}
}

for (istem in 1:nrow(data.structure)) {
	if (is.na(data.structure$"How was plant identified?"[istem])) {
		data.structure$"How was plant identified?"[istem] <- "Visual id"
	}
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: Height / Flag5

data.structure$"Height" <- structure$"height_m"

for (istem in 1:nrow(structure)) {
	if (!is.na(structure$"height_m"[istem])) {
		data.structure$"Flag5" <- 6
	}
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: original identification

data.structure$"original identification" <- structure$code

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: Stem tag grouping

data.structure$"Stem tag grouping"[structure$ramet==1] <- "A"
data.structure$"Stem tag grouping"[structure$ramet!=1] <- "B"

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
############################################################################### Dynamics
data.dynamics <- data.structure
dynamics <- GDB$dynamics 

############:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Growth
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Census date
data.dynamics[,c("Census date","D","Flag1")] <- NA
second.census <- subset(GDB$dates, measuring_type=="growth_1")
census2Date <- second.census$measuring_date[match(data.structure$"Plot Name",second.census$plot)]
data.dynamics$"Census date" <- census2Date

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::D, POM, Flags1
plot.ind.gth <- paste(dynamics$growth$plot, dynamics$growth$ind, sep=".")
plot.ind.str <- paste(data.structure$"Plot Name",data.structure$"Tag number", sep=".")

data.dynamics$"D" <- round(dynamics$growth$DBH_cm_fin[match(plot.ind.str, plot.ind.gth)]*10)

for (istem in 1:nrow(data.dynamics)) {
	if (is.na(data.dynamics$"D"[istem])) {
		data.dynamics$"POM"[istem] <- NA
		data.dynamics$"Flag1"[istem] <- 0
	} else {
		data.dynamics$"POM"[istem] <- 130
		data.dynamics$"Flag1"[istem] <- "a"
	}
}

############:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Mortality
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Flags2
dynamics$mortality$Flag2 <- vector(, nrow(dynamics$mortality))
dynamics$mortality$Flag2 <- NA

for (imodmor in 1:nrow(dynamics$mortality)) {
	if (!is.na(dynamics$mortality$"mode.of.death"[imodmor])) {
		if (dynamics$mortality$"mode.of.death"[imodmor] == "partido") {
			dynamics$mortality$Flag2[imodmor] <- "b" 
		}	
		if (dynamics$mortality$"mode.of.death"[imodmor] == "perdido") {
			dynamics$mortality$Flag2[imodmor] <- "k" 
		}		
		if (dynamics$mortality$"mode.of.death"[imodmor] == "en pie") {
			dynamics$mortality$Flag2[imodmor] <- "a" 
		}
		if (dynamics$mortality$"mode.of.death"[imodmor] == "desaparecido") {
			dynamics$mortality$Flag2[imodmor] <- "k" 
		}	
		if (dynamics$mortality$"mode.of.death"[imodmor] == "quebrado-caido") {
			dynamics$mortality$Flag2[imodmor] <- "h" 
		}	
		if (dynamics$mortality$"mode.of.death"[imodmor] == "caido") {
			dynamics$mortality$Flag2[imodmor] <- "c" 
		}		
		if (dynamics$mortality$"mode.of.death"[imodmor] == "caido, partido") {
			dynamics$mortality$Flag2[imodmor] <- "h" 
		}						
		if (dynamics$mortality$"mode.of.death"[imodmor] == "en pie, partido") {
			dynamics$mortality$Flag2[imodmor] <- "e" 
		}			
		if (dynamics$mortality$"mode.of.death"[imodmor] == "talado") {
			dynamics$mortality$Flag2[imodmor] <- "j" 
		}
		if (dynamics$mortality$"mode.of.death"[imodmor] == "Principal caida") {
			dynamics$mortality$Flag2[imodmor] <- "b" 
		}
		if (dynamics$mortality$"mode.of.death"[imodmor] == "partido, caido") {
			dynamics$mortality$Flag2[imodmor] <- "h" 
		}			
		if (dynamics$mortality$"mode.of.death"[imodmor] == "en pie ") {
			dynamics$mortality$Flag2[imodmor] <- "a" 
		}			
		if (dynamics$mortality$"mode.of.death"[imodmor] == "En pie ") {
			dynamics$mortality$Flag2[imodmor] <- "a" 
		}
	} else {
		if (is.na(dynamics$mortality$"mode.of.death"[imodmor])) {
		dynamics$mortality$Flag2[imodmor] <- "m" 
		} 	
	}			
}	

plot.ind.mor <- paste(dynamics$mortality$plot, dynamics$mortality$ind, sep=".")
plot.ind.str <- paste(data.structure$"Plot Name",data.structure$"Tag number", sep=".")

data.dynamics$"Flag2" <- dynamics$mortality$Flag2[match(plot.ind.str, plot.ind.mor)]
data.dynamics$"Flag2"[is.na(data.dynamics$"Flag2")] <- 1

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Flags3-4

data.dynamics$"Flag3"[which(data.dynamics$"Flag2" == 1)] <- NA
data.dynamics$"Flag4"[which(data.dynamics$"Flag2" == 1)] <- NA

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::How was plant identified?
data.dynamics$"How was plant identified?" <- "Homologated to previous census"
data.dynamics$"How was plant identified?"[which(data.dynamics$"Flag2" != 1)] <- NA

data.dynamics$"voucher code" <- NA
data.dynamics$"voucher collected" <- NA

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Height
data.dynamics$"Height"[which(data.dynamics$"Flag2" != 1)] <- NA
data.dynamics$"Flag5"[which(data.dynamics$"Flag2" != 1)] <- NA

############::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Recruits

recrutiment <- as.data.frame(matrix(NA, nrow=nrow(dynamics$recruitment), ncol=length(data.colnames)))
colnames(recrutiment) <- data.colnames

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Plot Name
recrutiment$"Plot Name" <- dynamics$recruitment$plot

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Census date
second.census <- subset(GDB$dates, measuring_type=="recruitment_1")

census2Date <- second.census$measuring_date[match(recrutiment$"Plot Name",second.census$plot)]
recrutiment$"Census date" <- census2Date
recrutiment$"Tag number" <- dynamics$recruitment$ind

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Family / Species
plot.code.rcr <- paste(dynamics$recruitment$plot,dynamics$recruitment$code, sep=".")
plot.code.tax <- paste(GDB$taxonomy$plot,GDB$taxonomy$code, sep=".")

family <- droplevels(GDB$taxonomy$family[match(plot.code.rcr,plot.code.tax)])
genus <- droplevels(GDB$taxonomy$genus[match(plot.code.rcr,plot.code.tax)])
sp_epithet <- droplevels(GDB$taxonomy$sp_epithet[match(plot.code.rcr,plot.code.tax)])

recrutiment$"Family" <- family
recrutiment$"Species" <- paste(genus, sp_epithet, sep=" ")

#::::::::::::::::::::::::::::::::::::::::::::::D, POM, Flags1-4, How was plant identified?
recrutiment$"D" <- round(dynamics$recruitment$DBH_cm_fin*10)
for (istem in 1:nrow(recrutiment)) {
	if (!is.na(recrutiment$"D"[istem])) {recrutiment$"POM"[istem] <- 130}
}
recrutiment$"Flag1" <- "a"
recrutiment$"Flag2" <- 1
recrutiment$"Flag3" <- 0
recrutiment$"Flag4" <- 0

recrutiment$"How was plant identified?" <- "Homologated to previous census"

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::Height

recrutiment$"Height" <- dynamics$recruitment$height_m

for (istem in 1:nrow(recrutiment)) {
	if (!is.na(recrutiment$"Height"[istem])) {
		recrutiment$"Flag5"[istem] <- 6
	}
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::original identification
recrutiment$"original identification" <- dynamics$recruitment$code

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: Stem tag grouping
recrutiment$"Stem tag grouping"[dynamics$recruitment$ramet==1] <- "A"
recrutiment$"Stem tag grouping"[dynamics$recruitment$ramet!=1] <- "B"

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: Corrected dynamicsD

for (istem in 1:nrow(data.dynamics)) {
	if (!is.na(data.dynamics$"D"[istem]) & !is.na(data.structure$"D"[istem])) {
		if ((data.dynamics$"D"[istem]-data.structure$"D"[istem]) < 0) {
			data.dynamics$"Tree Notes"[istem] <- paste("real D is [", data.dynamics$"D"[istem], "], but post-field data management generated negative growth, thus D was assumed as the previous census", sep="")
			data.dynamics$"D"[istem] <- data.structure$"D"[istem]
			data.dynamics$"Flag4"[istem] <- 7
		}
	}
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ASSEMBLE
dataSets <- rbind(data.structure,data.dynamics,recrutiment)
trees <- subset(GDB$taxonomy, c(life_form=="shrub" | life_form=="tree" | life_form=="treelet"))
trees.codes <- droplevels(unique(trees$code))
dataSets <- dataSets[which(dataSets$"original identification" %in% trees.codes),]

##########################################################################################
###################################################################Metadata in (J.Aguirre)
metadata.colnames <- c("PlotCode",
	"Plot Name",
	"Country",
	"Altitude",
	"Latitude (decimal degrees)"	,
	"Longitude (decimal degrees)",
	"Area (ha)",
	"Disturbance  Type (click on cell, then arrow, to select from drop down menu)",
	"Census dates",
	"Observations",
	"PIs",
	"Email",
	"PIscontact",
	"contact Email"
)

metadata <- GDB$plotsMetadata

data.metadata <- as.data.frame(matrix(NA, nrow=nrow(metadata), ncol=length(metadata.colnames)))
colnames(data.metadata) <- metadata.colnames

data.metadata$"Plot Name" <- metadata$plot
data.metadata$"Country" <- "Colombia"
data.metadata$"Altitude" <- metadata$altitude_m
data.metadata$"Latitude (decimal degrees)" <- metadata$latitude_dec
data.metadata$"Longitude (decimal degrees)" <- metadata$longitude_dec
data.metadata$"Area (ha)" <- metadata$area_ha
data.metadata$"Disturbance  Type (click on cell, then arrow, to select from drop down menu)" <- metadata$forest_status

stablishment <- subset(GDB$dates, measuring_type=="structure")
census_1 <- subset(GDB$dates, measuring_type=="growth_1")
for (iplot in 1:length(unique(GDB$dates$plot))) {
	temp.st <- stablishment$measuring_date[which(stablishment$plot==unique(GDB$dates$plot)[iplot])]
	temp.c1 <- census_1$measuring_date[which(census_1$plot==unique(GDB$dates$plot)[iplot])]
	data.metadata$"Census dates"[iplot] <- paste(temp.st, temp.c1, sep=", ")
}

data.metadata$"Observations" <- metadata$comments

members <- subset(GDB$members, c(sampling=="stablishment" | sampling=="census_1"))
membersPI <- subset(members, role_in_field=="Principle Researcher")

membersPI$"PIs" <- paste(membersPI$first_name, membersPI$last_name, sep=" ")
membersPI$"Email PIs" <- membersPI$email

membersPI <- membersPI[, c("plot", "sampling", "PIs", "Email PIs")]

membersPI.stablishment <- subset(membersPI, sampling=="stablishment")
membersPI.census_1 <- subset(membersPI, sampling=="census_1")

for (iplot in 1:length(unique(membersPI$plot))) {
	temp.PIst <- membersPI.stablishment$"PIs"[which(membersPI.stablishment$plot==unique(membersPI$plot)[iplot])]
	temp.PI.c1 <- membersPI.census_1$"PIs"[which(membersPI.census_1$plot==unique(membersPI$plot)[iplot])]
	tem.PI <- c(paste("stablishment: ", temp.PIst, ", ", sep=""), paste("census 1: ", temp.PI.c1, ", ", sep=""))
	tem.PI <- paste(tem.PI, sep = ", ", collapse = "")	
	data.metadata$"PIs"[iplot] <- tem.PI
	temp.mailsPIs <- droplevels(unique(membersPI$"Email PIs"[which(membersPI$PIs %in% unique(c(temp.PIst, temp.PI.c1)))]))
	data.metadata$"Email"[iplot] <- paste(temp.mailsPIs, sep = ",", collapse = ", ")	
	data.metadata$"PIscontact"[iplot] <- paste(unique(c(temp.PIst, temp.PI.c1)), sep = ",", collapse = ", ")	
	data.metadata$"contact Email"[iplot] <- data.metadata$"Email"[iplot]
}

metadataSet <- data.metadata
#############################################################################################
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ASSEMBLE
library(openxlsx)
wb <- loadWorkbook("~/Dropbox/dataForests/outputs/Templates/ForestsPlot_Inventory_Template.xlsx")
writeData(wb, sheet = "Tree-byTree_Inventory_data", x=dataSets)
writeData(wb, sheet = "Metadata", x=metadataSet)
saveWorkbook(wb,"~/Dropbox/dataForests/outputs/Output_Reports/Forestsplot_RedBSTCOL.xlsx")

#############################################################################################
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: CARDONAL

Cardonal.dataSets <- dataSets[which(dataSets$"Plot Name"== "CardonalLoma"), ]
Cardonal.metadata <- metadataSet[which(metadataSet$"Plot Name"== "CardonalLoma"), ]

wb <- loadWorkbook("~/Dropbox/dataForests/outputs/Templates/ForestsPlot_Inventory_Template.xlsx")
writeData(wb, sheet = "Tree-byTree_Inventory_data", x=Cardonal.dataSets)
writeData(wb, sheet = "Metadata", x=Cardonal.metadata)
saveWorkbook(wb,"~/Dropbox/dataForests/outputs/Output_Reports/Forestsplot_CardonalLoma.xlsx")

#############################################################################################
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: CARDONAL
Jabiru.dataSets <- dataSets[which(dataSets$"Plot Name"== "Jabiru"), ]
Jabiru.metadata <- metadataSet[which(metadataSet$"Plot Name"== "Jabiru"), ]

wb <- loadWorkbook("~/Dropbox/dataForests/outputs/Templates/ForestsPlot_Inventory_Template.xlsx")
writeData(wb, sheet = "Tree-byTree_Inventory_data", x=Jabiru.dataSets)
writeData(wb, sheet = "Metadata", x=Jabiru.metadata)
saveWorkbook(wb,"~/Dropbox/dataForests/outputs/Output_Reports/Forestsplot_Jabiru.xlsx")






