roysDate <- function(textDates)
{
  if(any(!is.na(textDates))) {
    aux.date <- as.character(textDates)
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
      res <- aux.eventDate 
    }
  } else {
    res <- textDates
  }	
  return(res)
}

manageDates<-function(textDates)
{
  NA
}