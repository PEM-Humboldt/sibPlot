
read_rawFile <- function(file, typeFile = c("csv","xlsx"))
{
  typeFile <- match.arg(typeFile)
  if (typeFile == "csv")
  {
    res <- list()
    res[[1]] <- read.csv(file, stringsAsFactors = F)
    names(res) <- sub("\\.csv","",basename(file))
  }
  return(res)
}
#######END######

read_rawFiles <- function(directory=NA , listFiles=NA, typeFiles = c("csv","xlsx"))
{
  if (is.na(directory) & is.na(listFiles))
  {
    stop("You must provide either a file list (as a character vector) or a directory")
  }
  if (any(!typeFiles %in% c("csv")))
  {
    stop(paste(typeFiles[!typeFiles %in% c("csv","xlsx")]," not implemented yet. If your format contains such files, please send an issue in github (marbotte/sibPlot)", collapse = "\n"))
  }
  if(is.na(listFiles))
  {listFiles <- paste(directory,dir(directory, recursive = T),sep = "/")}
  regexFiles <- as.list(paste0("\\.",typeFiles,"$"))
  names(regexFiles) <- typeFiles
  if (length(typeFiles) > 1)
  {
    reco_types <- test_regex_multi(listFiles, regexFiles)
    discard <- is.na(reco_types$ok)
    filesOK <- listFiles[!discard]
    types <- reco_types[!discard,"ok"]
  }else{
    discard <- !grepl(regexFiles, listFiles)
    filesOK <- listFiles[!discard]
    types <- rep(typeFiles, length(filesOK))
  }
  nbTypes <- table(types)
  message(length(filesOK)," files accepted (",paste(names(nbTypes),nbTypes,sep = ": ", collapse = ", "),", discarded: ", sum(discard),")")
  list_res <- list()
  subNames <- file_res <- character()
  for (i in 1:length(filesOK))
  {
    rf <- read_rawFile(filesOK[i],types[i])
    list_res <- append(list_res,rf)
    file_res <- c(file_res,rep(filesOK[i],length(rf)))
    subNames <- c(subNames, names(rf))
  }
  return(list(content = list_res, files = file_res, subNames = subNames))
}
###########END########

preformat_tables <- function(rawFiles, conn, formatName, formatVersion = "last")
{
  spec <- getInputSpec(conn, formatName, formatVersion)
  if (formatName == "BST_csv")
  {
    w_metadata <- Reduce(c,lapply(
      lapply(strsplit(spec$tables$regex_reco[spec$tables$tablename == "metadata"],";")[[1]],
           grepl,x = sub("^([A-Za-z0-9]+)_(.*)","\\1",rawFiles$subNames)),
      which))
    rawFiles$content[w_metadata] <- lapply(rawFiles$content[w_metadata],function(x)
    {
      x <- rbind(colnames(x),x)
      coln <- x[,1]
      res <- utils::type.convert(as.data.frame(t(x[,2:ncol(x)])))
      colnames(res) <- coln
      return(res)
    })
    return(rawFiles)
  } 
  stop("Preformatting not yet implemented for this input format")
}
##########END########

recog_tables <- function(rawFiles,conn,formatName,formatVersion = "last")
{
  spec <- getInputSpec(conn,formatName,formatVersion)
  # That works for BST_csv, but not sure whether it would work in other formats
  regexSubNames <- "^([A-Za-z0-9]+)_(.*)$"
  stopifnot(all(grepl(regexSubNames,rawFiles$subNames)))
  tablename <- sub(regexSubNames,"\\1",rawFiles$subNames)
  plotname <- sub(regexSubNames,"\\2",rawFiles$subNames)
  tabRecoRegex <- strsplit(spec$tables$regex_reco,";")
  names(tabRecoRegex) <- spec$tables$tablename
  recoTables <- test_regex_multi(tablename,tabRecoRegex,ignore.case = T)
  if (any(is.na(recoTables$ok)))
  {
    w <- which(is.na(recoTables$ok))
    warning(paste0("Table ",recoTables$testedString[w]," (from file: ",rawFiles$files[w],") was not recognized. If you want to add it to the format as an \"extra\" table, well it is not implemented yet, but this option will come soon. Otherwise you may want to check the name of the file"))
  }
  return(list(table = recoTables$ok, input_unit = plotname))
}
##########END######

input_sep <- function(rawFiles, recoTables, conn, formatName, formatVersion = "last")
{
  spec <- getInputSpec(conn_adm = conn, formatName = formatName, formatVersion = formatVersion)
  if (formatName == "BST_csv")
  {
    res <- list()
    un_plot <- unique(recoTables$input_unit)
    w_plot <- match(recoTables$input_unit, un_plot)
    tb_tab_plot <- table(recoTables$input_unit, recoTables$table)
    for (i in 1:length(un_plot))
    {
      plotCur <- un_plot[[i]]
      ind_tables <- which(recoTables$input_unit == plotCur)
      tables <- data.frame(tableType = recoTables$table[recoTables$input_unit == plotCur], ind_tables = ind_tables)
      tables$to_repeat <- tables$to_sep <- F
      tables$eventNumber <- NA
      if (any(table(tables$tableType) > 1))
      {stop("For plot: ",plotCur,", some tables types concern more than one table, please include them in different input processes (the earlier events first).")}
      w_dates <- which(recoTables$table[recoTables$input_unit == plotCur] == "dates")
      hasDates <- length(w_dates) == 1
      nbEventDates <- 0
      eventNumbers <- measNumberDates <- numeric()
      if (hasDates)
      {
        datesTab <- rawFiles$content[[which(recoTables$table == "dates" & recoTables$input_unit == plotCur)]]
        regexMeasuringTypes <- strsplit(spec$fields[spec$fields$fieldname == "measuring_type" & spec$fields$tablename == "dates","regex_reco"],";")[[1]]
        recoMeasuringTypes <- lapply(regexMeasuringTypes,grepl,x = colnames(datesTab))
        measType_ok <- sum(sapply(recoMeasuringTypes,sum)) == 1
        if (measType_ok)
        {
          measType_col <- Reduce(c,sapply(recoMeasuringTypes,which)) 
          nbEventDates <- nrow(datesTab)
          regexEvent <- "^ ?census[_ ]?([0-9]) ?$" #TODO : think  about other possibilities than "census"
          if (any(!grepl(regexEvent, datesTab[,measType_col])))
          {
            stop("rows ", paste0(which(!grepl(regexEvent, datesTab[,measType_col])), collapse=", "), " do not follow the authorized pattern for events in the measuring_type column (\"census0\", \"census1\", etc)")
          }
          eventNumbers <- measNumberDates <- as.numeric(sub(regexEvent,"\\1",datesTab[,measType_col],ignore.case = T))
          tables$to_sep[tables$tableType == "dates"] <- length(measNumberDates) > 1
          if (length(measNumberDates) == 1)
          {tables$eventNumber[tables$tableType == "dates"] <- measNumberDates
          }else
          {
            sep_dates <- by(datesTab,datesTab[,measType_col],function(x)x)
            names(sep_dates) <- sapply(sep_dates,function(x,r,col)paste0("census",sub(r,"\\1",x[1,col],ignore.case = T)),r = regexEvent, col = measType_col)
          }
        } else {
          hasDates <- F
        }
      }
      if (!hasDates)
      {
        warning("For plot: ", plotCur, " we did not find the \"dates\" table (or its format was not recognised). The dates table helps to separate the database inputs. It is recommended to add this table and to make sure it has a column \"measuringType\" or \"measuring_type\".")
      }
      hasMembers <- any(tables$tableType == "members")
      if (hasMembers)
      {
        membersTab <- rawFiles$content[[tables$ind_tables[tables$tableType == "members"]]]
        regexSampling <- strsplit(spec$fields[spec$fields$fieldname == "sampling" & spec$fields$tablename == "members","regex_reco"],";")[[1]]
        recoSampling <- lapply(regexSampling,grepl,x = colnames(membersTab))
        sampOk <- sum(sapply(recoSampling,sum)) == 1
        if (sampOk)
        {
          samp_col <- Reduce(c,sapply(recoSampling,which))
          regexEvent <-  "^ ?census[_ ]?([0-9]) ?$" #TODO : think  about other possibilities than "census"
          if (any(!grepl(regexEvent, membersTab[,samp_col])))
          {
            stop("rows ", paste0(which(!grepl(regexEvent, membersTab[,samp_col])), collapse=", "), " do not follow the authorized pattern for events in the sampling column (\"census0\", \"census1\", etc)")
          }
          membEventNumb <- unique(as.numeric(sub(regexEvent, "\\1", membersTab[,samp_col], ignore.case = T )))
          eventNumbers <- c(eventNumbers, membEventNumb)
          tables$to_sep[tables$tableType == "members"] <- length(membEventNumb) > 1
          if (length(membEventNumb) == 1)
          {tables$eventNumber[tables$tableType == "members"] <- membEventNumb
          }else{
            sep_members <- by(membersTab,membersTab[,samp_col],function(x)x)
            names(sep_members) <- sapply(sep_members,function(x,r,col)paste0("census",sub(r,"\\1",x[1,col],ignore.case = T)),r = regexEvent,col=samp_col)
          }
        }
      }
      tabCensusN <- any(tables$tableType %in% c("growth","mortality","recruitment"))
      if (tabCensusN)
      {
        w_ts <- which(tables$tableType %in% c("growth","mortality","recruitment"))
        regexTabCensus <- "^([A-Za-z]+)([0-9]+)_(.*)$"
        stopifnot(all(grepl(regexTabCensus, rawFiles$subNames[tables$ind_tables[w_ts]])))
        numCensusN <- as.numeric(sub(regexTabCensus,"\\2",rawFiles$subNames[tables$ind_tables[w_ts]]))
        eventNumbers <- c(eventNumbers,numCensusN)
        tables$eventNumber[w_ts] <- numCensusN
      }
      hasCensus0 <- any(tables$tableType == "census0")
      if (hasCensus0)
      {
        eventNumbers <- c(eventNumbers,0)
        tables$to_repeat[tables$tableType == "census0"] <- length(unique(eventNumbers)) > 1
      }
      rawFiles$subNames[tables$ind_tables]
      if (nbEventDates > 1 && !tabCensusN) # NB: nbEventDates > 1 implies hasDates
      {
        warning("For plot: ", plotCur, ", there are more than one event in the \"dates\" file, but no table are found for recruitment, growth, or mortality, please make sure all files for this plot were included")
      }
      hasTaxonomy <- any(tables$tableType == "taxonomy")
      if (hasTaxonomy){
        tables$to_repeat[tables$tableType == "taxonomy"] <- length(unique(eventNumbers)) > 1
      }
      if (length(unique(eventNumbers)) > 1 && (any(eventNumbers > 1) | !any(eventNumbers == 0)))
      {
        stop("Multiple events in the batch of sent files is only authorized for census0 and the first re-sampling, please separate the other events (see plot: ", plotCur,"). NB: This is due to the obligation of having all previous data in the database for the plot in order to check integrity of inserted data")
      }
      for (j in unique(eventNumbers))
      {
        res_j <- list()
        res_j$content <- list()
        if (j == 0)
        {
          res_j$content <- append(res_j$content,rawFiles$content[tables$ind_tables[!tables$to_sep & (is.na(tables$eventNumber) | tables$eventNumber == 0)]])
          summTable <- data.frame(
            tableType = tables$tableType[!tables$to_sep & (is.na(tables$eventNumber) | tables$eventNumber == 0)],
            file = rawFiles$files[tables$ind_tables[!tables$to_sep & (is.na(tables$eventNumber) | tables$eventNumber == 0)]],
            separated = F,
            repeated = F)
          if (any(tables$to_sep))
          {
            if (hasDates && tables$to_sep[tables$tableType == "dates"] && (0 %in% measNumberDates))
            {
              res_j$content$dates <- sep_dates$census0
              summTable <- rbind(summTable,
                                 data.frame(
                                   tableType = "dates",
                                   file = rawFiles$files[tables$ind_tables[tables$tableType == "dates"]],
                                   separated = T,
                                   repeated = F
                                 ))
            }
            if (hasMembers && tables$to_sep[tables$tableType == "members"] && (0 %in% membEventNumb))
            {
              res_j$content$members <- sep_members$census0
              summTable <- rbind(summTable,
                                 data.frame(
                                   tableType = "members",
                                   file = rawFiles$files[tables$ind_tables[tables$tableType == "members"]],
                                   separated = T,
                                   repeated = F
                                 ))
            }
          }
          names(res_j$content) <- summTable$tableType
          res_j$summary <- summTable
          res_j$event <- c(plot = plotCur, event = "census0")
        } else {
          cond <- !tables$to_sep & ( tables$to_repeat | tables$eventNumber == j)
          cond[is.na(cond)] <- F
          res_j$content <- append(res_j$content,rawFiles$content[tables$ind_tables[cond]])
          summTable <- data.frame(
            tableType = tables$tableType[cond],
            file = rawFiles$files[tables$ind_tables[cond]],
            separated = F,
            repeated = tables$to_repeat[cond])
          if (any(tables$to_sep))
          {
            if (hasDates && tables$to_sep[tables$tableType == "dates"] && (j %in% measNumberDates))
            {
              res_j$content$dates <- sep_dates[[paste0("census",j)]]
              summTable <- rbind(summTable,
                                 data.frame(
                                   tableType = "dates",
                                   file = rawFiles$files[tables$ind_tables[tables$tableType == "dates"]],
                                   separated = T,
                                   repeated = F
                                 ))
            }
            if (hasMembers && tables$to_sep[tables$tableType == "members"] && (j %in% membEventNumb))
            {
              res_j$content$members <- sep_members[[paste0("census",j)]]
              summTable <- rbind(summTable,
                                 data.frame(
                                   tableType = "members",
                                   file = rawFiles$files[tables$ind_tables[tables$tableType == "members"]],
                                   separated = T,
                                   repeated = F
                                 ))
            }
          }
        names(res_j$content) <- summTable$tableType
        res_j$summary <- summTable
        res_j$event <- c(plot = plotCur, event=paste("census",j,sep = ""))
        }
      res[[length(res) + 1]] <- res_j
      }
    }
  return(res)
  }
}
####### END #######

tryConversion <- function(vec, convert_to)
{
  test <- try(as(vec, convert_to))
  error <- class(test) == "try-error"
  if (!error)
  {
    pbs <- which(is.na(test) & !is.na(vec))
    return(list(ok = length(pbs) == 0, error = F, pbs = pbs, converted = test))
  } else {
    return(list(ok = F, error = T, pbs = NA, converted = vec))
  }
  
}

NAfy_emptyString <- function(char)
{
  char[grep("^ *$",char)] <- NA 
  return(char)
}
NAfy_emptyString_tab <- function(tab)
{
  colChar <- which(sapply(tab,class) == "character")
  for(i in colChar)
  {
    tab[,i] <- NAfy_emptyString(tab[,i])
  }
  return(tab)
}


checkMandatTables <- function(listError=list(), spec, input)
{
# Check presence of mandatory tables
  listError$mandatoryTables <- list()
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  listError$mandatoryTables$ok <- all(spec$tables$tablename[spec$tables$mandatory] %in% names(input$content))
  listError$mandatoryTables$pb <- spec$tables$tablename[!spec$tables$tablename[spec$tables$mandatory] %in% names(input$content)]
  return(listError)
}

checkRecognizeColumns <- function(listError=list(), spec, input)# NB: this function may modify the input
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  listError$unRecoCol <- list(
    ok = T,
    pb = data.frame(cd_tab = integer(), tablename = character(), column = character())
  )
  for (i in 1:length(cdTables))
  {
    regexRecoFields <- strsplit(spec$fields$regex_reco[spec$fields$cd_tab == cdTables[i]],";")
    names(regexRecoFields) <- spec$fields$fieldname[spec$fields$cd_tab == cdTables[i]]
    trm <- test_regex_multi(colnames(input$content[[i]]),regexRecoFields)
    colnames(input$content[[i]])[!is.na(trm$ok)] <- trm$ok[!is.na(trm$ok)]
    if (any(is.na(trm$ok)))
    {
      listError$unRecoCol$ok <- F
      listError$unRecoCol$pb <- rbind(listError$unRecoCol$pb,
                                      data.frame(
                                        cd_tab = cdTables[i],
                                        tablename = nameTables[i],
                                        column = trm$testedString[is.na(trm$ok)]
                                      ))
    }
  }
  return(list(listError = listError, input = input))
}


checkMandatoryFields <- function(listError=list(), spec, input)
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  mandaFields <- spec$fields[spec$fields$cd_tab %in% cdTables & spec$fields$mandatory, c("cd_tab","tablename","id_field","fieldname")]
  listError$mandatFieldExist <- list(ok = T, pb = data.frame(cd_tab = integer(),tablename = character(),cd_field = integer() ,fieldname=character()))
  listError$mandatFieldNa <- list(ok = T, pb = data.frame(cd_tab = integer(),tablename = character() ,fieldname = character(),row = integer()))
  for (cd_tab in unique(mandaFields$cd_tab))
  {
    # the following line transform any cell containing only spaces into NA
    curTab <- input$content[[which(cdTables == cd_tab)]]
    mandaCur <- mandaFields[mandaFields$cd_tab == cd_tab,]
## Check whether the field exists
    if (listError$mandatFieldExist$ok)
    {listError$mandatFieldExist$ok <- all(mandaCur$fieldname %in% colnames(curTab))}
    cond <- (!mandaCur$fieldname %in% colnames(curTab))
    listError$mandatFieldExist$pb <- rbind(listError$mandatFieldExist$pb,
      data.frame(cd_tab = mandaCur$cd_tab[cond],
                 tablename = mandaCur$tablename[cond],
                 cd_field = mandaCur$id_field[cond],
                 fieldname = mandaCur$fieldname[cond]
    ))
## Check on null values in mandatory fields
    mandaCur <- mandaCur[!cond,]
    checkNa <- lapply(curTab[mandaCur$fieldname],function(x)which(is.na(x)))
    listError$mandatFieldNa$pb <- rbind(listError$mandatFieldNa$pb, data.frame(
      cd_tab = rep(rep(cd_tab,length(checkNa)),sapply(checkNa,length)),
      tablename = rep(mandaCur$tablename[mandaCur$cd_tab == cd_tab],sapply(checkNa,length)),
      fieldname = rep(names(checkNa),sapply(checkNa,length)),
      row = Reduce(c,checkNa)
      ))
    if(listError$mandatFieldNa$ok)
    {listError$mandatFieldNa$ok <- !as.logical(nrow(listError$mandatFieldNa$pb))}
  }
  return(listError)
}

# TODO: We've got a problem, numeric are converted without errors to integer without making any warnings or error and transform for instance, 132.5 to 132... In some particular cases of the "ind" column of the census0 file, it might be problematic
checkTypeOfColumns <- function(listError=list(), specTypeR, input)# this function may modify the input
{
  nameTables <- input$summary$tableType
  cdTables <- specTypeR$tables$id_tab[match(nameTables,specTypeR$tables$tablename)]
  listError$typeofFullError <- list(ok = T, pb = data.frame(tablename = character(), fieldname = character(), type = character()))
  listError$typeofRowError <- list(ok = T, pb = data.frame(tablename = character(), fieldname = character(),type = character(), row = integer()))
  for (i in 1:length(cdTables))
  {
    specTypeof <- specTypeR$fields$typeofR[specTypeR$fields$cd_tab == cdTables[i]]
    names(specTypeof) <- specTypeR$fields$fieldname[specTypeR$fields$cd_tab == cdTables[i]]
    commonFields <- intersect(colnames(input$content[[i]]),names(specTypeof))
    for (j in commonFields)
    {
      tc <- tryConversion(input$content[[i]][,j],specTypeof[j])
      if (tc$ok)
      {input$content[[i]][,j] <- tc$converted
      } else {
        if (tc$error)
        {listError$typeofFullError$pb <- rbind(listError$typeofFullError$pb,
                                         data.frame(
                                           tablename = names(input$content[i]),
                                           fieldname = j,
                                           type = specTypeof[j]
                                         )
        )
        } else {
          listError$typeofRowError$pb <- rbind(listError$typeofRowError$pb,
                                         data.frame(
                                           tablename = rep(names(input$content[i]),length(tc$pbs)),
                                           fieldname = rep(j,length(tc$pbs)),
                                           type = rep(specTypeof[j],length(tc$pbs)),
                                           row = tc$pbs,
                                           value = input$content[[i]][tc$pbs,j])
                                         )
        }
      }
    }
    listError$typeofFullError$ok <- !as.logical(nrow(listError$typeofFullError$pb))
    listError$typeofRowError$ok <- !as.logical(nrow(listError$typeofRowError$pb))
  }
  return(list(listError=listError,input = input))
  
}

checkMaxChar <- function(listError=list(), spec, input)
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  listError$maxChar<-list(ok=T,pb = data.frame())
  for (i in 1:length(cdTables))
  {
    specMaxChar <- spec$fields$max_char[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$max_char)]
    names(specMaxChar) <- spec$fields$fieldname[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$max_char)]
    commonFields <- intersect(names(specMaxChar), colnames(input$content[[i]]))
    if (length(commonFields) == 0) {next}
    res <- mapply(function(x,y)which(!is.na(y) & nchar(as.character(y)) > x),specMaxChar[commonFields],input$content[[i]][commonFields],SIMPLIFY = F)
    tmpDf <- data.frame(tablename = rep(nameTables[i], sum(sapply(res,length))),
                        fieldname = rep(names(res), sapply(res,length)),
                        maxChar = rep(unname(specMaxChar[commonFields]),sapply(res,length)),
                        row = Reduce(c,res))
    tmpDf$value <- input$content[[i]][as.matrix(cbind(row=tmpDf$row,col=match(tmpDf$fieldname,colnames(input$content[[i]]))))]
    listError$maxChar$pb <- rbind(listError$maxChar$pb,tmpDf)
  }
  listError$maxChar$ok<-(!nrow(listError$maxChar$pb))
  return(listError)
}

checkMinNum <- function(listError=list(), spec, input)
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  listError$minNum<-list(ok=T,pb = data.frame())
  for (i in 1:length(cdTables))
  {
    specMinNum <- spec$fields$min_num[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$min_num)]
    names(specMinNum) <- spec$fields$fieldname[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$min_num)]
    commonFields <- intersect(names(specMinNum), colnames(input$content[[i]]))
    if (length(commonFields) == 0) {next}
    res <- mapply(function(x,y)which(!is.na(y) & (y < x)),specMinNum[commonFields],input$content[[i]][commonFields],SIMPLIFY = F)
    tmpDf <- data.frame(tablename = rep(nameTables[i], sum(sapply(res,length))),
                        fieldname = rep(names(res), sapply(res,length)),
                        minNum = rep(unname(specMinNum[commonFields]),sapply(res,length)),
                        row = Reduce(c,res))
    tmpDf$value <- input$content[[i]][as.matrix(cbind(row=tmpDf$row,col=match(tmpDf$fieldname,colnames(input$content[[i]]))))]
    listError$minNum$pb <- rbind(listError$minNum$pb,tmpDf)
  }
  listError$minNum$ok <- (!nrow(listError$minNum$pb))
  return(listError)
}

checkMaxNum <- function(listError=list(), spec, input)
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  listError$maxNum <- list(ok = T,pb = data.frame())
  for (i in 1:length(cdTables))
  {
    specMaxNum <- spec$fields$max_num[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$max_num)]
    names(specMaxNum) <- spec$fields$fieldname[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$max_num)]
    commonFields <- intersect(names(specMaxNum), colnames(input$content[[i]]))
    if (length(commonFields) == 0) {next}
    res <- mapply(function(x,y)which(!is.na(y) & (y > x)),specMaxNum[commonFields],input$content[[i]][commonFields],SIMPLIFY = F)
    tmpDf <- data.frame(tablename = rep(nameTables[i], sum(sapply(res,length))),
                        fieldname = rep(names(res), sapply(res,length)),
                        maxNum = rep(unname(specMaxNum[commonFields]),sapply(res,length)),
                        row = Reduce(c,res))
    tmpDf$value <- input$content[[i]][as.matrix(cbind(row=tmpDf$row,col = match(tmpDf$fieldname,colnames(input$content[[i]]))))]
    listError$maxNum$pb <- rbind(listError$maxNum$pb,tmpDf)
  }
  listError$maxNum$ok <- (!nrow(listError$maxNum$pb))
  return(listError)
}

checkUniques <- function(listError = list(), spec, input)
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  listError$Uniques <- list(ok=T, pb=data.frame())
  for (i in 1:length(cdTables))
  {
    specUnique <- spec$fields$fieldname[spec$fields$cd_tab == cdTables[i] & spec$fields$unique]
    commonFields <- intersect(specUnique,colnames(input$content[[i]]))
    if(length(commonFields) == 0) {next}
    res <- lapply(input$content[[i]][commonFields],function(x)which(duplicated(x)))
    tmpDf <- data.frame(tablename = rep(nameTables[i], sum(sapply(res,length))),
                        fieldname = rep(names(res),sapply(res,length)),
                        row = Reduce(c, res))
    tmpDf$value <- input$content[[i]][as.matrix(cbind(row=tmpDf$row,col = match(tmpDf$fieldname,colnames(input$content[[i]]))))]
    listError$Uniques$pb <- rbind(listError$Uniques$pb,tmpDf)
  }
  listError$Uniques$ok <- (!nrow(listError$Uniques$pb))
  return(listError)
}

checkFieldRegexes <- function(listError = list(), spec, input)
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  listError$FieldRegexes <- list(ok=T, pb=data.frame())
  for (i in 1:length(cdTables))
  {
    specFieldRegex <- strsplit(spec$fields$regex_field[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$regex_field) & !grepl("^ *$",spec$fields$regex_field)],";")
    names(specFieldRegex) <- spec$fields$fieldname[spec$fields$cd_tab == cdTables[i] & !is.na(spec$fields$regex_field) & !grepl("^ *$",spec$fields$regex_field)]
    commonFields <- intersect(names(specFieldRegex),colnames(input$content[[i]]))
    if(length(commonFields) == 0) {next}
    res <- mapply(function(x,y){
     which(!Reduce(`|`,lapply(y,function(z,x)grepl(z,x),x = x)))
    },input$content[[i]][commonFields],specFieldRegex[commonFields],SIMPLIFY = F)
    tmpDf <- data.frame(tablename = rep(nameTables[i], sum(sapply(res,length))),
                        fieldname = rep(names(res),sapply(res,length)),
                        row = Reduce(c, res))
    tmpDf$value <- input$content[[i]][as.matrix(cbind(row=tmpDf$row,col = match(tmpDf$fieldname,colnames(input$content[[i]]))))]
    listError$FieldRegexes$pb <- rbind(listError$FieldRegexes$pb,tmpDf)
  }
  listError$FieldRegexes$ok <- (!nrow(listError$FieldRegexes$pb))
  return(listError)
}

checkReferences <- function(listError = list(), spec, input)
{
  nameTables <- input$summary$tableType
  cdTables <- spec$tables$id_tab[match(nameTables,spec$tables$tablename)]
  refToCheck <- spec$fields[spec$fields$cd_tab %in% cdTables & (!is.na(spec$fields$ref_table) | !is.na(spec$fields$ref_field)), c("tablename","fieldname","ref_table","ref_field")]
  refToCheck$table_referenced <- spec$tables$tablename[match(refToCheck$ref_table,spec$tables$id_tab)]
  refToCheck$field_referenced <- spec$fields$fieldname[match(refToCheck$ref_field,spec$fields$id_field)]
  #Do the referenced fields exist
  tabExists <- refToCheck$table_referenced %in% nameTables
  if(any(tabExists)){
  fieExists <- apply(refToCheck[tabExists,c("table_referenced","field_referenced")], 1,
                     function(x,l)x[2] %in% colnames(l[[x[1]]]),l = input$content)
  }else{fieExists <- rep(F,nrow(refToCheck))}
  listError$refTabExists <- list(ok=all(tabExists),pb = data.frame(refToCheck[tabExists,c("tablename","fieldname","table_referenced")]))
  listError$refFieldExists <- list(ok=all(fieExists),pb = data.frame(refToCheck[tabExists,c("tablename","fieldname","table_referenced","field_referenced")]))
  listError$missingRef <- list(ok = T,pb = data.frame())
  refToCheck <- refToCheck[tabExists & fieExists,]
  if (nrow(refToCheck))
  {
    res <- by(refToCheck,list(row = 1:nrow(refToCheck)),
       function(row,l){
         val <- l[[row$tablename]][,row$fieldname]
         ref <- l[[row$table_referenced]][,row$field_referenced]
         return(which(!is.na(val) & !val %in% ref))
       }, l = input$content, simplify = F)
    if (sum(sapply(res,length)) > 0)
    {
      listError$missingRef$pb <- data.frame(
        tablename = rep(refToCheck$tablename,sapply(res,length)),
        fieldname = rep(refToCheck$fieldname,sapply(res,length)),
        table_referenced = rep(refToCheck$table_referenced,sapply(res,length)),
        field_referenced = rep(refToCheck$field_referenced,sapply(res,length)),
        row = Reduce(c,res)
      )
      listError$missingRef$pb$missingValue <- rep(NA,nrow(listError$missingRef$pb))
      for (j in 1:nrow(listError$missingRef$pb))
      {
        r <- listError$missingRef$pb[j,]
        listError$missingRef$pb$missingValue[j] <- as.character(input$content[[r$tablename]][r$row,r$fieldname])
      }
    }
    listError$missingRef$ok <- (!nrow(listError$missingRef$pb))
  }
  return(listError)
}
  

checkRuleNotIn <- function(listError = list(), spec, input)
{
  notInRules <- spec$rules[spec$rules$typerule == "NOT IN",]
  extractTab1 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\1",notInRules$rule)
  extractTab2 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\3",notInRules$rule)
  extractFields1 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\2",notInRules$rule)
  extractFields2 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\4",notInRules$rule)
  sepFields1 <- strsplit(extractFields1, " *,? +")
  sepFields2 <- strsplit(extractFields2, " *,? +")
  sepFields1 <- lapply(sepFields1, gsub,pattern = "(^ *)|( *$)", replacement = "")
  sepFields2 <- lapply(sepFields2, gsub,pattern = "(^ *)|( *$)", replacement = "")
# Initialize errors
  listError$ruleNotInRefTabMissing <- listError$ruleNotInRefFieldMissing <- listError$ruleNotIn <-
    list(ok = T, pb = data.frame())
# Do referencing tables and fields exist?
  for (j in 1:length(extractTab1))
  {
    # test whether tab and fields concerned are present
    tab1Ok <- extractTab1[j] %in% names(input$content)
    field1Ok <- rep(F, length(sepFields1[[j]]))
    tab2Ok <- extractTab2[j] %in% names(input$content)
    field2Ok <- rep(F, length(sepFields2[[j]]))
    if (tab1Ok) {
      field1Ok <- sepFields1[[j]] %in% colnames(input$content[[extractTab1[j]]])
    }
    if (tab2Ok) {
      field2Ok <- sepFields2[[j]] %in% colnames(input$content[[extractTab2[j]]])
    }
    if (all(field1Ok))# Referencing fields ok! 
    {
      if (!tab2Ok)
      {
        listError$ruleNotInRefTabMissing$pb <- 
          rbind(listError$ruleNotInRefTabMissing$pb,
                data.frame(
                  rule = notInRules[j,"id_rule"],
                  missingTab = extractTab2[j]))
      }else{
        listError$ruleNotInRefFieldMissing$pb <-
          rbind(listError$ruleNotInRefFieldMissing$pb,
                data.frame(
                  rule = rep(notInRules[j,"id_rule"], sum(!field2Ok)),
                  referenced_tab = rep(extractTab2[j], sum(!field2Ok)),
                  missingField = sepFields2[[j]][!field2Ok]
                ))
      }
      if (all(field2Ok))
      {
        refing_fac <- interaction(input$content[[extractTab1[j]]][,sepFields1[[j]]],drop = T)
        refed_fac <- interaction(input$content[[extractTab2[j]]][,sepFields2[[j]]], drop = T)
        pb_cases <- which(!is.na(refing_fac) & refing_fac %in% refed_fac)
        listError$ruleNotIn$pb <- 
          rbind(listError$ruleNotIn$pb,
                data.frame(
                  rule = rep(notInRules[j,"id_rule"], length(pb_cases)),
                  rule_lb = rep(notInRules[j,"rule"], length(pb_cases)),
                  row = pb_cases,
                  value = refing_fac[pb_cases]
                ))
      }
    }
  }
  listError$ruleNotInRefTabMissing$ok <- !as.logical(nrow(listError$ruleNotInRefTabMissing$pb))
  listError$ruleNotInRefFieldMissing$ok <- !as.logical(nrow(listError$ruleNotInRefFieldMissing$pb))
  listError$ruleNotIn$ok <- !as.logical(nrow(listError$ruleNotIn$pb))
  return(listError)
}

checkRuleAllIdent <- function(listError = list(), spec, input)
{
  allIdentRules <- spec$rules[spec$rules$typerule == "ALL IDENTICAL" & spec$rule$tablename %in% names(input$content),]
  listError$ruleAllIdent <- listError$ruleAllIdentMissingField <- list(ok = T, pb = data.frame())
  if (nrow(allIdentRules))
  {
    for (j in 1:nrow(allIdentRules))
    {
      refing_fields <- strsplit(sub("^(.+) ?\\<IN\\>.*$","\\1",allIdentRules[j,"rule"])," *, *")[[1]]
      refing_fields <- gsub("(^ *)|( *$)","",refing_fields)
      refed_fields <- strsplit(sub("^.*\\<IN\\> ?(.*)$","\\1",allIdentRules[j,"rule"])," *, *")[[1]]
      refed_fields <- gsub("(^ *)|( *$)","",refed_fields)
      if (!all(refing_fields %in% colnames(input$content[[allIdentRules$tablename[j]]]))) {next}
      if (any(!refed_fields %in% colnames(input$content[[allIdentRules$tablename[j]]])))
      {
        missField <- refed_fields[!refed_fields %in% colnames(input$content[[allIdentRules$tablename]])]
        listError$ruleAllIndentMissingField$pb <- 
          rbind(ruleAllIdentMissingField$pb,
                data.frame(
                  cd_rule = rep(allIdentRules[j,"id_rule"],length(missField)),
                  tablename = rep(allIdentRules[j,"tablename"],length(missField)),
                  missingField = missField
                ))
      }else{
        refing_fac <- interaction(input$content[[allIdentRules[j,"tablename"]]][,refing_fields],drop = T)
        refed_fac <- interaction(input$content[[allIdentRules[j,"tablename"]]][,refed_fields], drop = T)
        uniq_in <- tapply(refing_fac,refed_fac,unique,simplify = F)
        ln_uniq_in <- sapply(uniq_in,length) 
        if (sum(ln_uniq_in) > length(ln_uniq_in))
        {
          listError$ruleAllIdent$pb <- rbind(listError$ruleAllIdent$pb,
            data.frame(
              cd_rule = rep(allIdentRules[j,"id_rule"], sum(ln_uniq_in[ln_uniq_in > 1])),
              tablename = rep(allIdentRules[j,"tablename"], sum(ln_uniq_in[ln_uniq_in > 1])),
              rule = rep(allIdentRules[j,"tablename"], sum(ln_uniq_in[ln_uniq_in > 1])),
              values = Reduce(c,uniq_in[ln_uniq_in > 1]),
              in_values = rep(names(uniq_in)[ln_uniq_in > 1],ln_uniq_in[ln_uniq_in > 1])
            )
          )
        }
      }
      
    }
  listError$ruleAllIdentMissingField$ok <- !as.logical(nrow(listError$ruleAllIdentMissingField$pb))
  listError$ruleAllIdent$ok <- !as.logical(nrow(listError$ruleAllIdent$pb))
  }
  return(listError)
}
  
checkRuleUnique <- function(listError = list(), spec, input)
{
  stop("UNIQUE rules not implemented yet")
}
checkRuleForeignMul <- function(listError = list(), spec, input)
{
  stop("Multiple foreign keys not implemented yet")
}

  
checkInputSelfIntegrity <- function(input, conn, formatName, formatVersion= "last")
{
  spec <- getInputSpec(conn, formatName, formatVersion)
# Transform into NA the empty strings
   input$content <- lapply(input$content,NAfy_emptyString_tab) 
#check presence of mandatory tables
  listError <- checkMandatTables(spec = spec,input = input)
# recognize columns
  recoCol <- checkRecognizeColumns(listError,spec,input)
  listError <- recoCol$listError
  input <- recoCol$input
  rm(recoCol) 
# Check mandatory fields
  listError <- checkMandatoryFields(listError,spec,input)
# Checking on typeof
  dbTypeof <- dbGetQuery(conn,"SELECT id_to,type_r FROM spec.def_typeof")
  spec$fields$typeofR <- dbTypeof$type_r[match(spec$fields$typeof,dbTypeof$id_to)]
  testTypeof <- checkTypeOfColumns(listError,spec,input)
  listError <- testTypeof$listError
  input <- testTypeof$input
  rm(testTypeof)
# Checking on maxCha
  listError <- checkMaxChar(listError, spec, input)
# Checking on minNum
  listError <- checkMinNum(listError, spec, input)
# Checking on maxNum
  listError <- checkMaxNum(listError, spec, input)
# Checking on unique
  listError <- checkUniques(listError, spec, input)
# Checking on regexes
  listError <- checkFieldRegexes(listError, spec, input)
# Checking on foreign keys
  listError <- checkReferences(listError, spec, input)
# Checking on rules
  listError <- checkRuleNotIn(listError, spec, input)
  listError <- checkRuleAllIdent(listError, spec, input)
  if (any(spec$rules$id_rule == "UNIQUE") && any(spec$rules[spec$rules$typerule == "UNIQUE","tablename"] %in% names(input$content)))
  {
  listError <- checkRuleUnique(listError, spec, input)
  }
  if (any(spec$rules$id_rule == "FOREIGN MULTI") && any(spec$rules[spec$rules$typerule == "FOREIGN MULTI","tablename"] %in% names(input$content)))
  {
  listError <- checkRuleForeignMul(listError, spec, input)
  }
  
  return(list(selfInteg = listError, input = input))
}
  
  input <- sepInputs[[42]]
  conn <- sib_user
  formatName <- "BST_csv"
  formatVersion <- "last"

A <- data.frame(c("a","b","c",NA),c(1,2,3,4))
B <- data.frame(c("c","d","e","f"),c(3,4,5,NA))
fac_A <- interaction(A, drop = T)
fac_B <- interaction(B, drop = T)
fac_A %in% fac_B
match(fac_A, fac_B)
match(fac_B,fac_A)  
