dataRastrojos <- readRDS("~/Desktop/Bases de datos Rastrojos/temp.censo0/dataRastrojos.rds")

temp.PLOT_1 <- dataRastrojos$Census0_1

###########################################################I2D
data.colnames <- c("eventID","ID del organismo")
#################################################### Structure

plantilla <- as.data.frame(matrix(NA, nrow=nrow(temp.PLOT_1), ncol=length(data.colnames)))
colnames(plantilla) <- data.colnames





eventID