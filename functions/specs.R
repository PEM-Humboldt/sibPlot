
read_Xlspec <- function(file_spec)
{
  tb_spec <- openxlsx::read.xlsx(file_spec,sheet = "tableSpec")
  regex_tb <- strsplit(tb_spec$regex,";")
  names(regex_tb) <- tb_spec$typeTable
  fi_spec <- openxlsx::read.xlsx(file_spec,sheet = "fieldSpec")
  regex_fi <- strsplit(fi_spec$regex[!is.na(fi_spec$regex)],";")
  names(regex_fi) <- fi_spec$field[!is.na(fi_spec$regex)]
  fi_spec <- fi_spec[,c("table","field","example","typeof","maxChar","minNum","maxNum")]
  if (any(!fi_spec$table %in% tb_spec$typeTable))
  {
    stop(paste("In the field specs we found the following tables:", paste(unique(fi_spec$table[which(!fi_spec$table %in% tb_spec$typeTable)]),collapse = ", "),"which ARE NOT DEFINED in the table specs",sep = "\n"))
  }
  return(list(regex_tb = regex_tb,fi_spec = fi_spec,regex_fi = regex_fi))
}
############END###########


read_CsvSpec <- function(fileSpecTab, fileSpecField, fileSpecExtra=NA, fileSpecPrefor=NA)
{
  tb_spec <- read.csv(fileSpecTab)
  regex_tb <- strsplit(tb_spec$regex,";")
  names(regex_tb) <- tb_spec$typeTable
  fi_spec <- read.csv(fileSpecField)
  recoField <- strsplit(fi_spec$recoField[!is.na(fi_spec$recoField) & !fi_spec$recoField == ""],";")
  names(recoField) <- fi_spec$field[!is.na(fi_spec$recoField) & !fi_spec$recoField == ""]
  regex_fi <- strsplit(fi_spec$regex[!is.na(fi_spec$regex) & !fi_spec$regex == ""],";")
  names(regex_fi) <- fi_spec$field[!is.na(fi_spec$regex) & !fi_spec$regex == ""]
  fi_spec <- fi_spec[,c("table", "field", "recoField", "mandatory", "example", "typeof", "maxChar", "minNum", "maxNum", "regex", "includedFormat")]
  if (any(!fi_spec$table %in% tb_spec$typeTable))
  {
    stop(paste("In the field specs we found the following tables:", paste(unique(fi_spec$table[which(!fi_spec$table %in% tb_spec$typeTable)]),collapse = ", "),"which ARE NOT DEFINED in the table specs",sep = "\n"))
  }
  
  pf_spec <- NULL
  if (!is.na(fileSpecPrefor))
  {
    tb_pf <- read.csv(fileSpecPrefor)
    recoField_pf <- strsplit(tb_pf$recoField[!is.na(tb_pf$recoField) & !tb_pf$recoField == ""],";")
    names(recoField_pf) <- tb_pf$field[!is.na(tb_pf$recoField) & !tb_pf$recoField == ""]
    recoField_pf <- tapply(recoField_pf,tb_pf$table[!is.na(tb_pf$recoField) & !tb_pf$recoField == ""],function(x)x,simplify = F)
    pf_spec <- list(tb_pf = tb_pf, recoField_pf = recoField_pf)
  }
  return(list(regex_tb = regex_tb, fi_spec = fi_spec, recoField = recoField, regex_fi = regex_fi, pf_spec = pf_spec))
}
#################### END ###################

# Note: regex spec is a list of 2 levels
# The names of the first level gives the resulting results (which is the field or table which is compatible)
# the index of the second level give the specific regex which resulted true
# The function checks whether there is only one regex being true (error if not)
# and then send a table with the regex which works if any and the index in the regex group
test_regex_multi <- function(vecChar,regex_spec,...)
{
  if (any(duplicated(names(regex_spec)))) {warning(paste0("\"", unique(names(regex_spec)[duplicated(names(regex_spec))]), "\"",sep = "",collapse = ", ")," are repeated through the names of the regexes")}
  mat_res<-matrix(NA,nrow=length(vecChar),ncol=sum(sapply(regex_spec,length)))
  counter <- 0
  ok <- rep(names(regex_spec),sapply(regex_spec,length))
  nb <- Reduce(c,lapply(regex_spec,function(x)1:length(x)))
  for (i in 1:length(regex_spec))
  {for (j in 1:length(regex_spec[[i]]))
    {
    counter<-counter+1
    mat_res[,counter]<-grepl(regex_spec[[i]][j],vecChar)#,...)
    }
  }
  res0 <- lapply(as.data.frame(t(mat_res)),which)
  names(res0) <- vecChar
  nbOk <- sapply(res0,function(x,o){length(unique(o[x]))},o=ok)
  nbRegexT <- sapply(res0,length)
  if (any(nbRegexT > 1 & !nbOk > 1))
  {warning("Multiple regexes corresponded (but all sending the same result) for the following strings:\n",paste("\"",vecChar[nbRegexT > 1 & !nbOk > 1],"\"",sep="",collapse=", "),"\n the first corresponding regex is returned")}
  if(any(nbOk > 1))
  {stop("\nMultiple results in the regex recognition process for the following entered strings:\n", paste("\"", vecChar[nbOk>1], "\"", sep = "", collapse = ", "))
  }
  res <- data.frame(testedString = vecChar, 
    Reduce(rbind,sapply(res0,function(x,o,n)
    {
      if (length(x) == 0) {return(data.frame(ok = NA,nb = NA))}
      return(data.frame(ok = unique(o[x]), nb = min(n[x])))
      }, o = ok, n = nb))
  )
  return(res)
}

##########END###########

test_regex<-function(vecChar,vecRegex,...)
{
  mat_res<-matrix(nrow=length(vecChar),ncol=length(vecRegex))
  for(i in 1:length(vecRegex))
  {mat_res[,i]<-grepl(vecRegex[i],vecChar,...)}
  if(any(rowSums(mat_res)>1))
  {
    stop()# To do: make stop message
  }else{
    return(apply(mat_res,1,function(x)
    {a<-which(x)
    res<-ifelse(length(a)==0,NA,a)
    }))
  }
}
#########END############

analyseCsvFiles<-function(dos=NA,regex_types,files=NA)
{
  if(all(is.na(files))&all(is.na(dos))){stop("Either dos or files is required")}
  if(!all(is.na(files))&!all(is.na(dos))){warning("Both files and dos given: files will override dos")}
  # if dos is given but not files, the function will recursively search for csv files in directory and subdirectories
  if(all(is.na(files))){
    files<-paste(dos,dir(dos,recursive=T,pattern=".*\\.csv$"),sep="/")
  }
  pureNames<-sub("^(.*)\\.csv$","\\1",basename(files))
  # extract types and plot
  regex_typesplot<-"^(.+)_(.*)$"
  types<-sub(regex_typesplot,"\\1",pureNames)
  plots<-sub(regex_typesplot,"\\2",pureNames)
  types_cat<-test_regex_multi(unique(types),regex_types)
  res_cat<-types_cat[match(types,types_cat$testedString),]
  return(data.frame(file = files, plot = factor(plots), type = types, type_cat = factor(res_cat$ok), w_regex = res_cat$nb))
}
#########################END####################


readCsvFiles<-function(files,categ)
{
  stopifnot(length(files)==length(categ))
  res<-mapply(function(x,y)
  {
    if(y=="metadata")
    {
      return(data.frame(t(read.csv(x,row.names = 1,header=F)),stringsAsFactors = F))
    }else{
      return(read.csv(x,header = T,stringsAsFactors = F))
    }
  },files,categ)
  return(res)
}
##########END#################


listInputFormats <- function(conn_adm)
{
  dbGetQuery(conn_adm,"SELECT * FROM spec.in_format")
}
##############END#############


maxVersion <- function(versions)
{
  if(length(versions)==1){return(versions)}
  spl <- lapply(strsplit(versions,"\\."),as.integer)
  max_lev <- max(sapply(spl,length))
  tabVersion <- as.data.frame(Reduce(rbind,lapply(spl, function(x,ml) c(x, rep(0,ml - length(x))),ml = max_lev)))
  ORD <- do.call(order, tabVersion)
  return(versions[which.max(ORD)])
}
##########END#############

getInputSpec <- function(conn_adm, formatName, formatVersion = "last")
{
  formats <- dbGetQuery(conn_adm,
    "SELECT *
    FROM spec.in_format
    WHERE formatname=$1",
  params=list(formatName))
  stopifnot(formatName %in% formats$formatname)
  if(formatVersion == "last")
  {formatVersion <- maxVersion(formats$version)}
  res <- list()
  res$format <- dbGetQuery(conn_adm,
      "SELECT *
      FROM spec.in_format
      WHERE formatname=$1 AND version=$2
      ",params=list(formatName,formatVersion))
  res$tables <- dbGetQuery(conn_adm,
      "SELECT tb.*
      FROM spec.in_format fo
        JOIN spec.in_rel_tab irt ON fo.id_for=irt.cd_for
        JOIN spec.in_tables tb ON irt.cd_tab=tb.id_tab
      WHERE formatname=$1 AND version=$2",
      params = list(formatName, formatVersion))
  res$fields <- dbGetQuery(conn_adm,
      "SELECT fi.id_field, fi.cd_tab, tb.tablename, fi.fieldname, fi.example, fi.regex_reco, fi.typeof, fi.unit, fi.max_char, fi.min_num, fi.max_num, fi.mandatory, fi.extra, fi.regex_field, fi.ref_table, fi.ref_field, fi.lev_ref, fi.comment
      FROM spec.in_format fo
        JOIN spec.in_rel_field irt ON fo.id_for=irt.cd_for
        JOIN spec.in_fields fi ON irt.cd_field=fi.id_field
        JOIN spec.in_tables tb ON fi.cd_tab=tb.id_tab
      WHERE formatname=$1 AND version=$2
      ORDER BY fi.cd_tab, fi.ordercol
      ",
      params = list(formatName, formatVersion))
  res$rules <- dbGetQuery(conn_adm,
      "SELECT ru.id_rule, ru.typerule, ru.cd_tab, tb.tablename, ru.rule, ru.comment
      FROM spec.in_format fo
        JOIN spec.in_rel_rule irl ON fo.id_for=irl.cd_for
        JOIN spec.in_rule ru ON irl.cd_rule=ru.id_rule
        JOIN spec.in_tables tb ON ru.cd_tab=tb.id_tab
      WHERE formatname=$1 AND version=$2",
      params = list(formatName, formatVersion))
  res$requirements <- dbGetQuery(conn_adm,
      "SELECT cd_tr, requirement
      FROM spec.in_format fo
        JOIN spec.in_requi re ON fo.id_for=re.cd_for
      WHERE formatname=$1 AND version=$2",
      params = list(formatName, formatVersion))
  res$functions <- dbGetQuery(conn_adm,
      "SELECT if.*
      FROM spec.in_format fo
        JOIN spec.in_rel_func irf ON fo.id_for=irf.cd_for
        JOIN spec.in_functions if ON irf.cd_func=if.id_func
      WHERE formatname=$1 AND version=$2",
      params = list(formatName, formatVersion))
  return(res)
}
############END############

indentVersion <- function(version,typeUpdate = c("minor","major"))
{
  stopifnot(length(version)==1)
  typeUpdate <- match.arg(typeUpdate)
  sep_ver <- as.integer(strsplit(version,"\\.")[[1]])
  if (typeUpdate == "major")
  {
    sep_ver[1] <- sep_ver[1] + 1
    sep_ver[2:length(sep_ver)] <- 0
  }
  if (typeUpdate == "minor")
  {
    sep_ver[length(sep_ver)] <- sep_ver[length(sep_ver)] + 1
  }
  return(paste(sep_ver,collapse = "."))
}
############END############


prepareInputFormat <- function(conn_adm, formatName, author = NA, typeUpdate = c("minor", "major", "new"), fromFormat = ifelse(typeUpdate %in% c("minor","major"), formatName, NA), fromVersion = "last", dirName = "./sibPlot_inputFormats/", typeFile=c("xlsx","csv"), newVersion = NA)
{
  # argument management
  typeUpdate <- match.arg(typeUpdate)
  if(is.na(fromFormat))
  {stop("Please indicate the format you want to use as a model (argument *fromFormat*)")}
  if(fromVersion == "last")
  {
    fromVersion <- maxVersion(dbGetQuery(conn_adm, "SELECT * FROM spec.in_format WHERE formatname=$1", params=list(fromFormat))$version)
  }
  typeFile <- match.arg(typeFile)
  if (!dir.exists(dirName))
  {dir.create(dirName)}
  # extracting model
  res <- getInputSpec(conn_adm, fromFormat, fromVersion)
  # extracting format 
  if (typeUpdate %in% c("minor","major") & is.na(newVersion))
  {formatVersion <- indentVersion(fromVersion,typeUpdate)}
  if (typeUpdate == "new")
  {formatVersion <- "1.0"}
  res$format <- data.frame(formatname = formatName,version = formatVersion, createdBy = author, description = res$format$description)
  #TODO: make an explanation sheet for excel, or an explanation textFile for csv or... send a link to a document from gitHub which explains how to do
  
  # writing files
  message("Escribiendo archivos...")
  if(typeFile == "xlsx")
  {
    wb <- openxlsx::createWorkbook()
    for(i in 1:length(res))
    {
      openxlsx::addWorksheet(wb,sheetName = names(res)[i])
      openxlsx::writeData(wb, sheet = names(res)[i],
                               x = res[[i]],
                               keepNA = F,
                               withFilter = F
                               )
      openxlsx::setColWidths(wb, names(res)[i], cols = 1:ncol(res[[i]]), widths = "auto")
      #TODO: Here we could add styles to help the users understand what they should do with the excel file
    }
    fileName <- paste(dirName,paste0(formatName,"_",formatVersion,".xlsx"),sep="/")
    openxlsx::saveWorkbook(wb, file = fileName, overwrite = F)
    message("done!")
    return(list(dir = dirName, wb=wb, res = res))
  }else if (typeFile == "csv")
  {
    for (i in 1:length(res))
    {
      fileName <- paste(dirName,paste0(formatName,"_",formatVersion, "_", names(res)[i],".csv"),sep="/")
      if (file.exists(fileName)) {stop("File",filename,"already exists!")}
      write.csv(res[[i]],file = fileName, row.names = F,quote = T)
    }
    message("done!")
    return(list(dirName,res = res))
  }
  
}
############END############

removeInputSpec <- function(conn_adm, formatName, formatVersion="last", removeAll = F)
{
  if(formatVersion == "last")
  {
    formatVersion <- maxVersion(dbGetQuery(conn_adm,"SELECT version FROM spec.in_format WHERE formatname=$1",params = list(formatName))$version) 
  }
  res <- dbSendQuery(conn_adm,
              "DELETE FROM spec.in_format
              WHERE formatname=$1 AND version=$2",params=list(formatName,formatVersion))
  affected <- dbGetRowsAffected(res)
  dbClearResult(res)
  return(affected)
}
#############END############

getGarbageSpecInput <- function(conn_adm, returnedConcerned = T, clean = F, elements = c("tables","fields","rules","functions"))
{ # TODO 
  # The idea here is to have a function which look for the un-referenced (by formats) tables, fields, rules, functions in the input specification. If the clean option is True, then it removes them
}

#############END############

checkNewFormat <- function(conn_adm, mainFile, typeFile =c("xlsx","csv")){
  typeFile <- match.arg(typeFile)
  if (typeFile == "xlsx")
  {
    wb <- openxlsx::loadWorkbook(mainFile)
    spec <- lapply(names(wb), openxlsx::readWorkbook, xlsxFile = mainFile)
  } else if (typeFile == "csv")
  {
    dirName <- dirname(mainFile)
    Format <- read.csv(mainFile)
    formatName <- Format$formatname
    formatVersion <- Format$version
    regexFile <- paste0("^",formatName,"_",formatVersion,"_","(.+)","\\.csv")
    files <- dir(dirName, pattern = regexFile)
    spec <- lapply(paste(dirName, files, sep = "/"), function(x,a)
    {
      read <- read.csv(x)
      if (any(names(a) %in% colnames(read)))
      {read <- read.csv(x, colClasses = a[names(a) %in% colnames(read)])}
      return(read)
    },a = c(version = "character"))
    names(spec) <- sub(regexFile, "\\1", files)
  }
  message("Format name: ", spec$format$formatname)
  message("Format version: ", spec$format$version)
  pb <- list()
# Checking on elements
  elements <- c("format","tables","fields","functions","requirements","rules")
  missElem <- which(!elements %in% names(spec))
  message("Checking integrity of the format...")
  if (length(missElem) > 0)
  {pb[[length(pb) + 1]] <- list(messages = paste0(elements[missElem]," is missing in the format"),
                                 type = "Missing elements", importance = ifelse(any(1:3) %in% missElem, "stop0", "warning"))
    if (pb[[length(pb)]]$importance == "stop1") {stop(paste(paste(pb[[length(pb)]]$messages,collapse = "\n"),"'format', 'tables' and 'fields' are mandatory")) }
  }
# Checking and modifying spec tables
## Tables
### tablename
  if (any(is.na(spec$tables$tablename)))
  {
    pb[[length(pb + 1)]] <- list(message = paste0(sum(is.na(spec.tables$tablename))," table names are missing"),
                                 type = "Missing tablenames", importance = "stop0")
    return(pb)
  }
  if (any(is.na(spec$tables$regex_reco) | grepl("^ ?$",spec$tables$regex_reco)))
  {
    message(paste("Creating automatic regular expression for table recognition: ",spec$tables$fieldname[is.na(spec$tables$regex_reco) | grepl("^ ?$",spec$tables$regex_reco)],collapse = "\n",sep = ""))
    spec$tables$regex_reco[is.na(spec$tables$regex_reco) | grepl("^ ?$",spec$tables$regex_reco)] <- paste("^",spec$tables$tablename[is.na(spec$tables$regex_reco) | grepl("^ ?$",spec$tables$regex_reco)],"$",sep="")
  }
### Checking whether regex recognize the tables
  regexRecoTab <- strsplit(spec$tables$regex_reco,split = ";")
  names(regexRecoTab) <- spec$tables$tablename
  trm <- test_regex_multi(spec$tables$tablename, regexRecoTab)
  if (any(trm$testedString != trm$ok))
  {
    stop(paste0(trm$testedString[trm$testedString != trm$ok]," is not recognized as such by its own recognition regex", collapse = "\n"))
  }
### Checking that the id_tab are unique
  if(any(duplicated(spec$tables$id_tab)))
  {stop(paste0(spec$tables$id_tab[duplicated(spec$tables$id_tab)]," is not unique", collapse = "\n"))}
## Fields
### Checking whether tables codes are present in the table
  if (any(!spec$fields$cd_tab %in% spec$tables$id_tab))
  {
    stop(paste0(spec$fields$cd_tab[!spec$fields$cd_tab %in% spec$tables$id_tab], " is not present in the id_tab column of the table specifications"))
  }
### Checking whether cd_tab and table names corresponds in the field table
  tableNamesFromTab <- spec$tables$tablename[match(spec$fields$cd_tab,spec$tables$id_tab)]
  if (any(spec$fields$tablename != tableNamesFromTab))
  {
    w <- which(spec$fields$tablename != tableNamesFromTab)
    stop(paste0("cd_tab",spec$fields$cd_tab[w], " does not correspond to its table name (",spec$fields$tablename," instead of ",tableNamesFromTab[w],")"))
  }
### Checking whether fieldnames are unique in a tablename
  dupFields <- tapply(spec$fields$fieldname, spec$fields$tablename, function(x) x[duplicated(x)])
  dupFields_tab <- rep(names(dupFields), sapply(dupFields, length))
  dupFields <- Reduce(c, dupFields)
  if (length(dupFields) > 0)
  {
    stop("The following field names are duplicated:\n",paste0(dupFields," (table: ", dupFields_tab,")", collapse = ", "))
  }
### Checking whether the fieldnames are recognized by their regex_reco
  if (any(is.na(spec$fields$regex_reco) | grepl("^ ?$",spec$fields$regex_reco)))
  {
    message(paste0("Creating automatic regular expression for field recognition: ", spec$fields$fieldname[is.na(spec$fields$regex_reco) | grepl("^ ?$",spec$fields$regex_reco)], collapse = "\n"))
    spec$fields$regex_reco[is.na(spec$fields$regex_reco) | grepl("^ ?$",spec$fields$regex_reco)] <- paste("^",spec$fields$fieldname[is.na(spec$fields$regex_reco) | grepl("^ ?$",spec$fields$regex_reco)],"$", sep = "")
  }
  byRes <- by(spec$fields,spec$fields$cd_tab,function(tab){
    regexes <- strsplit(tab$regex_reco,";")
    names(regexes) <- tab$fieldname
    trm <- test_regex_multi(tab$fieldname,regexes)
    if (any(trm$testedString != trm$ok | is.na(trm$ok)))
    {
      stop(paste0(trm$testedString[trm$testedString != trm$ok | is.na(trm$ok)]," is not recognized as such by its own recognition regex", collapse = "\n"))}
    else {return(0)}
  })
### Checking whether information given for the max_char, min_num, max_num, regex_field correspond to their typeof
  authorized_types <- dbGetQuery(sib_adm, "SELECT id_to FROM spec.def_typeof")$id_to
  unrecognised_typeof <- spec$fields[is.na(spec$fields$typeof) | !spec$fields$typeof %in% authorized_types,c("tablename","fieldname","typeof")]
  if (nrow(unrecognised_typeof) > 0)
  {
    stop(paste0("Field \"", unrecognised_typeof$fieldname, "\" of table \"", unrecognised_typeof$tablename, "\" has an unrecognized typeof: \"",unrecognised_typeof$typeof,"\"",collapse = "\n"),"\nnote: authorized types are: ", paste0(authorized_types,collapse=", "))
  }
  nonsenseMaxChar <- spec$fields[!is.na(spec$fields$max_char) & spec$fields$typeof != "varchar", c("tablename","fieldname","typeof")]
  if (nrow(nonsenseMaxChar))
  {
    warning("The following fields have a maximum number of characters but are not of type 'varchar':\n",paste0("Field ",nonsenseMaxChar$fieldname," of table ",nonsenseMaxChar$tablename, " (",nonsenseMaxChar$typeof,")",collapse="\n"), "\nthe max_char rule is deleted")
    spec$fields$max_char[!is.na(spec$fields$max_char) &! spec$fields$typeof == "varchar"] <- NA 
  }
  missingMaxChar <- spec$fields[is.na(spec$fields$max_char) & spec$fields$typeof == "varchar", c("tablename", "fieldname")]
  if (nrow(missingMaxChar) > 0)
    {stop("The following fields do not have a maximum number of characters but are of type 'varchar':\n",paste0("Field ",missingMaxChar$fieldname," of table ",missingMaxChar$tablename,collapse="\n"), "\nPlease add a value in the 'max_char' specification")}
  nonsenseMinNum_MaxNum <- spec$fields[(!is.na(spec$fields$min_num) | !is.na(spec$fields$max_num)) & !spec$fields$typeof %in% c("integer", "double precision"),c("tablename","fieldname","min_num","max_num")] 
  if (nrow(nonsenseMinNum_MaxNum) > 0)
  {
    warning("The following fields have a minimum or a maximum value but are not of numeric type:\n",paste0("Field ",nonsenseMinNum_MaxNum$fieldname," of table ",nonsenseMinNum_MaxNum$tablename, " (min: ",nonsenseMinNum_MaxNumr$min_num," max: ",nonsenseMinNum_MaxNum$max_num,")",collapse="\n"), "\nthe min_num and max_num rule is deleted")
    spec$fields$min_num[!is.na(spec$fields$min_num) & !spec$fields$typeof %in% c("integer", "double precision")] <- NA 
    spec$fields$max_num[!is.na(spec$fields$max_num) & !spec$fields$typeof %in% c("integer", "double precision")] <- NA 
  }
  nonsenseRegex <- spec$fields[!(is.na(spec$fields$regex_field) | grepl("^ ?$",spec$fields$regex_field)) & !spec$fields$typeof %in% c("varchar", "text"),c("tablename", "fieldname", "typeof")]
  if (nrow(nonsenseRegex) > 0)
  {
    warning("The following fields have a regex but are not of string type:\n",paste0("Field ",nonsenseRegex$fieldname, " of table ",nonsenseRegex$tablename,collapse = "\n"), "\nthe regex rule rule is deleted")
    spec$fields$regex_field[!(is.na(spec$fields$regex_field) | grepl("^ ?$",spec$fields$regex_field)) & !spec$fields$typeof %in% c("varchar", "text")] <- NA
  }
### adding orderCol
  if (!"ordercol" %in% colnames(spec$fields) | ("ordercol" %in% colnames(spec$fields) & all(is.na(spec$fields$ordercol))))
  {
    message("Creating a variable including the order of the fields in the table (depending on their order in the read file)")
    spec$fields$ordercol <- NA
    for (i in 1:nrow(spec$fields))
    {
      counter <- max(spec$fields$ordercol[spec$fields$tablename == spec$fields[i,"tablename"]]) + 1
      spec$fields[i,"ordercol"] <- counter
    }
  } 
### Checking on references
  isNumRefTab <- is.numeric(spec$fields$ref_table)
  isNumRefCol <- is.numeric(spec$fields$ref_field)
  if (!all(is.na(c(spec$fields$ref_table,spec$fields$ref_field))) & (!isNumRefCol | !isNumRefCol))
  {
    stop("References in column 'ref_table' and 'ref_field' should be numeric (integer) and reference the codes of the tables and fields. We detect: ",paste(sapply(spec$fields[c("ref_table","ref_field")],class), collapse = ", "))
  }
  spec$fields$ref_table[is.na(spec$fields$ref_table) & !is.na(spec$fields$ref_field)] <- spec$fields$id_tab[match(spec$fields$ref_field[is.na(spec$fields$ref_table) & !is.na(spec$fields$ref_field)],spec$fields$id_field),"id_tab"]
  w <- which(!is.na(spec$fields$ref_table) | !is.na(spec$fields$ref_field))
  refs <- spec$fields[w,c("id_field","ref_table","ref_field")]
  stopifnot(!any(is.na(refs)))
  tabOk <- spec$field[match(refs$ref_field,spec$fields$id_field),"cd_tab"] == refs$ref_table
  if (any(!tabOk))
  {
    warning(paste("Referenced table for field",refs[id_field[!tabOk]],"does not correspond to referenced field",collapse = "\n"),"\nTable references are changed according to field references")
    spec$fields$ref_table[w[!tabOk]] <- spec$fields$cd_tab[match(refs$ref_field,spec$fields$id_field)]
  }
### Checking on mandatory
  stopifnot(is.logical(spec$fields$mandatory))
  spec$fields$mandatory[is.na(spec$fields$mandatory)] <- F
### Checking on unique
  stopifnot(is.logical(spec$fields$unique))
  spec$fields$unique[is.na(spec$fields$unique)] <- F
### Checking on extra
  stopifnot(is.logical(spec$fields$extra))
  spec$fields$extra[is.na(spec$fields$extra)] <- F
  spec$fields$lev_ref <- as.character(spec$fields$lev_ref)
  extra <- spec$fields[spec$fields$extra,c("tablename","fieldname","lev_ref")]
  if (nrow(extra) > 0)
    if(any(is.na(extra$lev_ref)))
    {
      warning("Extra tables and fields need lev_ref defined in order to work correctly")
    }
  levRefAuthorized <- dbGetQuery(conn_adm,"SELECT id_lev_ref FROM spec.def_lev_ref")$id_lev_ref
  if (any(!extra$lev_ref %in% levRefAuthorized))
  {
    w <- which(!extra$lev_ref %in% levRefAuthorized)
    warning("The following fields have an 'extra' characteristic but their lev_ref is not defined:\n",paste0("Field ",extra$fieldname[w], " of table ",extra$tablename[w],collapse = "\n"), "\nproblems might appear when we will habilitate the extra tables and fields")
  }
## Comments
  spec$fields$comment <- as.character(spec$fields$comment)
#TODO: it does not make sense to reference a field with a different typeof, and it does not make sense to reference a field that is not varchar or integer (even though it might happen with text, date or time)
### Checking on examples
#TODO: define a way to make examples and check that examples follow it.
##rules 
#TODO I am hesitating between cd and names for the tables and fields. Since we checked everything in tables and fields before, and that fields and tables will be automatically extractable from the database in the storage formats (potentially more SQL universal than cd as defined here), we might want to use the names instead... It would make sense as well for the references defined before... But at the same time why bother putting any cd_tab and cd_field in the input specifications if we do not use them... they might actually be better excluded from the input specification forms, or used... I have to take the time to think it through...
  if("rules" %in% names(spec) & nrow(spec$rules) > 0)
  {
    typeRuleAuthorized <- dbGetQuery(sib_adm,"SELECT typerule FROM spec.def_rule WHERE in_implemented")$typerule
    if (any(!spec$rules$typerule %in% typeRuleAuthorized))
    {
      w <- which(!spec$rules$typerule %in% typeRuleAuthorized)
      stop(paste0("Rule number ",spec$rules$id_rule[w]," is not of any type authorized (",spec$rules$typerule[w],")", collapse = "\n"),"\nAuthorized types are:",paste0(typeRuleAuthorized, collapse = ", "))
    }
    if (any(is.na(spec$rules$cd_tab) | !spec$rules$cd_tab %in% spec$tables$id_tab))
    {
      w <- which(is.na(spec$rules$cd_tab) | !spec$rules$cd_tab %in% spec$tables$id_tab)
      stop("\n",paste("Rule number",spec$rules$id_rule[w],"references an undefined table(",spec$rules$cd_tab,")", collapse = "\n"))
    }
    if (any(spec$rules$type_rule == "UNIQUE"))
    {
      w <- which(spec$rules$type_rule == "UNIQUE")
      tabs <- spec$rules$cd_tab[w]
      regex <- "^UNIQUE ?\\(([A-Za-z_0-9]+ ?, ?)*([A-Za-z_0-9]+)\\)"
      if (any(!grepl(regex,spec$rules$rule[w])))
      {
        w2 <- which(!grepl(regex,spec$rules$rule[w]))
        stop(paste0("Rule ", spec$rules$id_rule[w][w2], " does not follow the format for UNIQUE rules (example \"UNIQUE(field1, field2, field3)\"", collapse = "\n"))
      }
      extractFields <- sub("^UNIQUE ?\\((.*)\\)","\\1",spec$rules$rule[w])
      sepFields <- strsplit(extractFields, ",")
      sepFields <- lapply(sepFields, gsub,pattern = "(^ *)|( *$)", replacement = "")
      tabFields <- data.frame(
        rule = rep(spec$rules$id_rule[w], sapply(sepFields,length)),
        tab = rep(spec$rules$cd_tab[w], sapply(sepFields, length)),
        fields = Reduce(c, sepFields)
        )
      m <- apply(tabFields,1,function(x,t) {
        wf <- which(t$cd_tab == x[2] & t$fieldname == x[3])
        return(ifelse(length(wf) == 0, NA, wf))
        }, t=spec$fields)
      if (any(is.na(m)))
      {stop(paste0("UNIQUE rule number ", tabFields$rule[is.na(m)], " references an inexisting field (\"", tabFields$fields[is.na(m)],"\") in table \"", tabFields$tab[is.na(m)],"\"",collapse = "\n"))}
    }
    if (any(spec$rules$type_rule == "ALL IDENTICAL"))
    {
      w <- which(spec$rules$type_rule == "ALL IDENTICAL")
      tabs <- spec$rules$cd_tab[w]
      regex <- "^([A-Za-z_0-9]+ ?, ?)*([A-Za-z_0-9]+) IN ([A-Za-z_0-9]+ ?, ?)*([A-Za-z_0-9]+)$"
      if (any(!grepl(regex,spec$rules$rule[w])))
      {
        w2 <- which(!grepl(regex,spec$rules$rule[w]))
        stop(paste0("Rule ", spec$rules$id_rule[w][w2], " does not follow the format for ALL IDENTICAL rules (example \"field1 IN field2,field3)\"", collapse = "\n"))
      }
      extractFields <- sub("IN","",spec$rules$rule[w])
      sepFields <- strsplit(extractFields, " +,? *")
      sepFields <- lapply(sepFields, gsub,pattern = "(^ *)|( *$)", replacement = "")
      tabFields <- data.frame(
        rule = rep(spec$rules$id_rule[w], sapply(sepFields,length)),
        tab = rep(spec$rules$cd_tab[w], sapply(sepFields, length)),
        fields = Reduce(c, sepFields)
        )
      m <- apply(tabFields,1,function(x,t) {
        wf <- which(t$cd_tab == x[2] & t$fieldname == x[3])
        return(ifelse(length(wf) == 0, NA, wf))
        }, t=spec$fields)
      if (any(is.na(m)))
      {stop(paste0("ALL IDENTICAL rule number ", tabFields$rule[is.na(m)], " references an inexisting field (\"", tabFields$fields[is.na(m)],"\") in table \"", tabFields$tab[is.na(m)],"\"",collapse = "\n"))}
    }
    if (any(spec$rules$type_rule == "FOREIGN MULTI"))
    {
      w <- which(spec$rules$type_rule == "FOREIGN MULTI")
      tabs <- spec$rules$cd_tab[w]
      regex <- "^[A-Za-z0-9_]+ ?\\(([A-Za-z_0-9]+ ?, ?)*([A-Za-z_0-9]+)\\) REFERENCES [A-Za-z0-9_]+ ?\\((([A-Za-z_0-9]+ ?, ?)*([A-Za-z_0-9]+)\\)$"
      if (any(!grepl(regex,spec$rules$rule[w])))
      {
        w2 <- which(!grepl(regex,spec$rules$rule[w]))
        stop(paste0("Rule ", spec$rules$id_rule[w][w2], " does not follow the format for FOREIGN MULTI rules (example \"tab1(field1, field2) REFERENCES tab2(field3, field4)\"", collapse = "\n"))
      }
      extractTab1 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) REFERENCES ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\1",spec$rules$rule[w])
      extractTab2 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) REFERENCES ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\3",spec$rules$rule[w])
      extractFields1 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) REFERENCES ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\2",spec$rules$rule[w])
      extractFields2 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) REFERENCES ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\4",spec$rules$rule[w])
      sepFields1 <- strsplit(extractFields1, " *,? +")
      sepFields2 <- strsplit(extractFields2, " *,? +")
      sepFields1 <- lapply(sepFields1, gsub,pattern = "(^ *)|( *$)", replacement = "")
      sepFields2 <- lapply(sepFields2, gsub,pattern = "(^ *)|( *$)", replacement = "")
      if (any(sapply(sepFields1, length) != sapply(sepFields2, length)))
      {
        w2 <- which(sapply(sepFields1, length) != sapply(sepFields2, length))
        stop(paste0("In FOREIGN MULTI rule ", spec$rules$id_rule, ", numbers of referencing and referenced fields are not the same",collapse = "\n"))
      }
      tabFields <- data.frame(
        rule = c(rep(spec$rules$id_rule[w], sapply(sepFields1,length)),rep(spec$rules$id_rule[w], sapply(sepFields2,length))),
        tab = c(rep(extractTab1, sapply(sepFields1, length)), rep(extractTab2,sapply(sepFields2, length))),
        fields = c(Reduce(c, sepFields1),Reduce(c, sepFields2))
        )
      m <- apply(tabFields,1,function(x,t) {
        wf <- which(t$tablename == x[2] & t$fieldname == x[3])
        return(ifelse(length(wf) == 0, NA, wf))
        }, t=spec$fields)
      if (any(is.na(m)))
      {stop(paste0("FOREIGN MULTI rule number ", tabFields$rule[is.na(m)], " references an inexisting field (\"", tabFields$fields[is.na(m)],"\") in table \"", tabFields$tab[is.na(m)],"\"",collapse = "\n"))}
    }
    if (any(spec$rules$type_rule == "NOT IN"))
    {
      w <- which(spec$rules$type_rule == "NOT IN")
      tabs <- spec$rules$cd_tab[w]
      regex <- "^[A-Za-z0-9_]+ ?\\(([A-Za-z_0-9]+ ?, ?)*([A-Za-z_0-9]+)\\) NOT IN [A-Za-z0-9_]+ ?\\(([A-Za-z_0-9]+ ?, ?)*([A-Za-z_0-9]+)\\)$"
      if (any(!grepl(regex,spec$rules$rule[w])))
      {
        w2 <- which(!grepl(regex,spec$rules$rule[w]))
        stop(paste0("Rule ", spec$rules$id_rule[w][w2], " does not follow the format for NOT IN rules (example \"tab1(field1, field2) NOT IN tab2(field3, field4)\"", collapse = "\n"))
      }
      extractTab1 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\1",spec$rules$rule[w])
      extractTab2 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\3",spec$rules$rule[w])
      extractFields1 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\2",spec$rules$rule[w])
      extractFields2 <- gsub("^([A-Za-z0-9_]+) ?\\((.*)\\) NOT IN ([A-Za-z0-9_]+) ?\\((.*)\\)$","\\4",spec$rules$rule[w])
      sepFields1 <- strsplit(extractFields1, " *,? +")
      sepFields2 <- strsplit(extractFields2, " *,? +")
      sepFields1 <- lapply(sepFields1, gsub,pattern = "(^ *)|( *$)", replacement = "")
      sepFields2 <- lapply(sepFields2, gsub,pattern = "(^ *)|( *$)", replacement = "")
      if (any(sapply(sepFields1, length) != sapply(sepFields2, length)))
      {
        w2 <- which(sapply(sepFields1, length) != sapply(sepFields2, length))
        stop(paste0("In NOT IN rule ", spec$rules$id_rule, ", numbers of referencing and referenced fields are not the same",collapse = "\n"))
      }
      tabFields <- data.frame(
        rule = c(rep(spec$rules$id_rule[w], sapply(sepFields1,length)),rep(spec$rules$id_rule[w], sapply(sepFields2,length))),
        tab = c(rep(extractTab1, sapply(sepFields1, length)), rep(extractTab2,sapply(sepFields2, length))),
        fields = c(Reduce(c, sepFields1),Reduce(c, sepFields2))
        )
      m <- apply(tabFields,1,function(x,t) {
        wf <- which(t$tablename == x[2] & t$fieldname == x[3])
        return(ifelse(length(wf) == 0, NA, wf))
        }, t = spec$fields)
      if (any(is.na(m)))
      {stop(paste0("NOT IN rule number ", tabFields$rule[is.na(m)], " references an inexisting field (\"", tabFields$fields[is.na(m)],"\") in table \"", tabFields$tab[is.na(m)],"\"",collapse = "\n"))}
      
    }
  }
  message("Verification is over... You may proceed with new format integration!")
#TODO
##requirement
#TODO
  return(spec)
}
##############END###########

prepareQueryId <- function(conn, query, params, identifiers = logical(length(params))) 
{
  stopifnot(length(query) == 1)
  stopifnot(length(identifiers) == length(params))
  extractParam <- regmatches(query,gregexpr("\\$[0-9]+",query))[[1]]
  stopifnot(1:length(params) %in% as.numeric(gsub("\\$([0-9]+)","\\1",extractParam)))
  if (any(identifiers))
  {
    idParams <- paste0("$",which(identifiers))
    for (i in 1:length(idParams))
    {
      query <- gsub(sub("\\$","\\\\$",idParams[i]), dbQuoteIdentifier(conn,params[[which(identifiers)[i]]]), query)
    }
  }
  if (any(!identifiers))
  {
    changeParamNumbers <- cbind(before = which(!identifiers), after = 1:sum(!identifiers))
    for (i in 1:nrow(changeParamNumbers))
    {
      query <- gsub(paste0("\\$",changeParamNumbers[i,"before"]),paste0("$", changeParamNumbers[i,"after"], query))
    }
    params <- params[!identifiers]
  }else{
    params <- NULL
  }
  return(list(conn=conn, statement = query, params = params))
}
###########END############




insertNewInFormat <- function(spec, conn_adm)
{
# Writing temporary tables
  dbBegin(conn_adm)
  for (i in 1:length(spec))
  {dbWriteTable(conn_adm, name = c("spec",paste0("tmp_",names(spec)[i])), value = spec[[i]])}
  
# checking format name and version
  for_query <- "SELECT f.formatname IS NULL AS new,formatname, version, COALESCE(f.created_by,n.created_by) created_by , COALESCE(f.description,n.description) description
  FROM spec.tmp_format n
  LEFT JOIN spec.in_format f USING (formatname, version)"
  forNewOld <- dbGetQuery(conn_adm, for_query)
  if (sum(forNewOld$new) != 1)
  {
    dbRollback(conn_adm)
    stop("There are ",sum(forNewOld$new)," new format(s).")
  }
  ins_for_que <- paste0(
    "WITH new_old AS(",for_query,")",
    "INSERT INTO spec.in_format
    (formatname, version, created_by, creation_date, install_date,  description)
    SELECT formatname, version, created_by, NOW(), NOW(), description
    FROM new_old
    WHERE new")
  res <- dbSendStatement(sib_adm,ins_for_que)
  dbClearResult(res)
  
# Inserting and updating tables
  tab_query <- 
    "SELECT t.id_tab IS NULL as new, t.id_tab id_tab_db, tt.id_tab, tablename, mandatory,regex_reco, extra,  tt.comment::text, tt.comment IS NOT NULL AND NOT t.comment=tt.comment::text AS comment_upd 
     FROM spec.tmp_tables tt 
     LEFT JOIN spec.in_tables t USING(tablename,mandatory,regex_reco,extra)
     ORDER BY id_tab
  "
  tabNewOld <- dbGetQuery(conn_adm, tab_query)
  ins_tab_que <- paste0(
    "WITH new_old AS (", tab_query,")
    INSERT INTO spec.in_tables(tablename, mandatory, regex_reco, extra, comment)
    SELECT tablename, mandatory, regex_reco, extra, comment
    FROM new_old
    WHERE new
    ORDER BY id_tab
    ")
  res <- dbSendStatement(conn_adm, ins_tab_que)
  tabNbNew <- dbGetRowsAffected(res)
  message(tabNbNew, " new table(s) inserted")
  dbClearResult(res)
  upd_tab_que <- paste0(
    "WITH new_old AS(",tab_query,")
    UPDATE spec.in_tables t
    SET comment=no.comment
    FROM new_old no
    WHERE comment_upd AND no.id_tab_db=t.id_tab
    ")
  res <- dbSendStatement(conn_adm, upd_tab_que)
  tabUpd <- dbGetRowsAffected(res)
  message(tabUpd, " table(s) updated")
  dbClearResult(res)
# Changing references to tables
  que_corres_tab <- paste(
    "CREATE TABLE spec.tmp_corres_tab AS(
    SELECT o.id_tab_db, o.id_tab
    FROM (", tab_query, ") AS o
    )")
  res <- dbSendStatement(conn_adm, que_corres_tab)
  dbClearResult(res)
  que_chg_cd_tab <- 
    "UPDATE spec.$1 t
    SET $2=c.id_tab_db
    FROM spec.tmp_corres_tab c
    WHERE t.$2=c.id_tab AND NOT c.id_tab_db = c.id_tab
    "
  q1 <- prepareQueryId(conn_adm, que_chg_cd_tab, params = list("tmp_fields","cd_tab"), identifiers = c(T,T))
  res <- do.call(dbSendQuery,q1)
  dbClearResult(res)
  q2 <- prepareQueryId(conn_adm, que_chg_cd_tab, params = list("tmp_fields","ref_table"), identifiers = c(T,T))
  res <- do.call(dbSendQuery,q2)
  dbClearResult(res)
  q3 <- prepareQueryId(conn_adm, que_chg_cd_tab, params = list("tmp_rules","cd_tab"), identifiers = c(T,T))
  res <- do.call(dbSendQuery,q3)
  dbClearResult(res)
# Putting tables in the format
  ins_tab_for_que <- "INSERT INTO spec.in_rel_tab
  SELECT id_for, id_tab_db
  FROM spec.in_format f , spec.tmp_corres_tab tct
  WHERE formatname=$1 AND version=$2
  "
  res <- dbSendQuery(conn_adm, ins_tab_for_que, params = list(spec$format$formatname,spec$format$version))
  dbClearResult(res)                       
# Inserting and updating fields
  if (dbGetQuery(conn_adm,"SELECT data_type FROM information_schema.columns WHERE table_name='tmp_fields' AND column_name='lev_ref'")$data_type == "boolean")
  {
    res <- dbSendStatement(conn_adm, "ALTER TABLE spec.tmp_fields ALTER lev_ref TYPE TEXT")
    dbClearResult(res)
  }
  if (dbGetQuery(conn_adm,"SELECT data_type FROM information_schema.columns WHERE table_name='tmp_fields' AND column_name='comment'")$data_type == "boolean")
  {
    res <- dbSendStatement(conn_adm, "ALTER TABLE spec.tmp_fields ALTER comment TYPE TEXT")
    dbClearResult(res)
  }
  #TODO We've got a problem with the self referencement in the table with ref_field, a change in this field should result in a new row, but we cannot know it until the rows are inserted
  select_fields <- "SELECT f.id_field IS NULL AS \"new\", f.id_field id_field_db, tf.id_field,
    tf.cd_tab, tf.fieldname, tf.ordercol, tf.example, tf.regex_reco, tf.typeof, tf.unit, tf.max_char, tf.min_num, tf.max_num, tf.\"unique\",tf.mandatory, tf.extra, tf.lev_ref, tf.regex_field, tf.ref_table, tf.ref_field, tf.comment, (tf.example IS NOT NULL AND NOT tf.example=f.example) OR (tf.comment IS NOT NULL AND NOT tf.comment=f.comment) OR (NOT tf.ordercol=f.ordercol ) AS upd
    FROM spec.tmp_fields tf"
  join_without_ref_field <- "LEFT JOIN spec.in_fields f ON(
  (tf.cd_tab=f.cd_tab) AND
           (tf.fieldname=f.fieldname) AND
           (tf.regex_reco=f.regex_reco OR (tf.regex_reco IS NULL AND f.regex_reco IS NULL)) AND
           (tf.typeof=f.typeof) AND
           (tf.unit=f.unit OR (tf.unit IS NULL AND f.unit IS NULL)) AND
           (tf.max_char=f.max_char OR (tf.max_char IS NULL AND f.max_char IS NULL)) AND
           (tf.min_num=f.min_num OR (tf.min_num IS NULL AND f.min_num IS NULL)) AND
           (tf.max_num=f.max_num OR (tf.max_num IS NULL AND f.max_num IS NULL)) AND
           (tf.\"unique\"=f.\"unique\" ) AND
           (tf.mandatory=f.mandatory ) AND
           (tf.extra=f.extra ) AND
           (tf.lev_ref=f.lev_ref OR (tf.lev_ref IS NULL AND f.lev_ref IS NULL)) AND
           (tf.regex_field=f.regex_field OR (tf.regex_field IS NULL AND f.regex_field IS NULL)) AND
           (tf.ref_table=f.ref_table OR (tf.ref_table IS NULL AND f.ref_table IS NULL)) /*HERE REPLACE*/
  )"
  join_complete <- sub("/\\*HERE REPLACE\\*/","AND
                       (tf.ref_field=f.ref_field OR (tf.ref_field IS NULL AND f.ref_field IS NULL))",
                       join_without_ref_field)
  field_query1 <- paste(select_fields, join_without_ref_field)
  field_query2 <- paste(select_fields, join_complete)
  ins_fie_que <- paste0("WITH new_old AS(",field_query1,")
    INSERT INTO spec.in_fields(cd_tab, fieldname, ordercol, example, regex_reco, typeof, unit, max_char, min_num, max_num, \"unique\", mandatory, extra, lev_ref, regex_field, ref_table, comment)
    SELECT cd_tab,fieldname, ordercol, example, regex_reco, typeof, unit, max_char, min_num, max_num, \"unique\", mandatory, extra, lev_ref, regex_field, ref_table, comment
    FROM new_old
    WHERE new
    ORDER BY cd_tab, ordercol
    RETURNING id_field
    ")
  res <- dbSendQuery(conn_adm, ins_fie_que)
  new_idFields <- dbFetch(res, n = -1)
  nbNewField <- length(new_idFields$id_field)
  message(nbNewField, " new field inserted")
  dbClearResult(res)
  upd_fie_que <- paste0(
    "WITH new_old AS(",field_query1,")
    UPDATE spec.in_fields f
    SET example=no.example,comment=no.comment, ordercol=no.ordercol
    FROM new_old no
    WHERE upd AND no.id_field_db=f.id_field    ")
  res <- dbSendStatement(conn_adm, upd_fie_que)
  fieldUpd <- dbGetRowsAffected(res)
  message(tabUpd, " field(s) updated")
  dbClearResult(res)
# Changing references to fields
  que_corres_field <- paste(
    "CREATE TABLE spec.tmp_corres_fie AS(
    SELECT f.id_field id_field_db, tf.id_field
    FROM ", sub(".*FROM(.*)$","\\1", field_query1) ,
    ")")
  res <- dbSendQuery(conn_adm, que_corres_field)
  dbClearResult(res)
  que_chg_ref_field <-
    "WITH new_ref_field AS(
      SELECT tc1.id_field_db id_field, tc2.id_field_db ref_field
      FROM spec.tmp_fields tf
      JOIN spec.tmp_corres_fie tc1 ON tf.id_field=tc1.id_field
      JOIN spec.tmp_corres_fie tc2 ON tf.ref_field=tc2.id_field
      WHERE ref_field IS NOT NULL
    )
    UPDATE spec.in_fields f
    SET ref_field=n.ref_field
    FROM new_ref_field n
    WHERE f.id_field=n.id_field
    "
  res <- dbSendQuery(conn_adm,que_chg_ref_field)
  dbClearResult(res)
  que_chg_ref_field_tmp <- 
    "WITH new_ref_field AS(
      SELECT tc1.id_field, tc1.id_field_db id_field_db, tc2.id_field_db ref_field
      FROM spec.tmp_fields tf
      LEFT JOIN spec.tmp_corres_fie tc1 ON tf.id_field=tc1.id_field
      LEFT JOIN spec.tmp_corres_fie tc2 ON tf.ref_field=tc2.id_field
    )
    UPDATE spec.tmp_fields tf
    SET id_field=n.id_field_db, ref_field=n.ref_field
    FROM new_ref_field n
    WHERE tf.id_field=n.id_field
    "
  res <- dbSendQuery(conn_adm, que_chg_ref_field_tmp)
  dbClearResult(res)
  completeJoin <- dbGetQuery(conn_adm,field_query2)
  if (any(completeJoin$new))
  {
    dbRollback(conn_adm)
    stop("Due to a particular problem of self-referencing table (the field table) in the format specification, you've entered a special case that the developers had foreseen but hoped would never happen, please contact them through github: marbotte/sibPlot, so they can resolve your problem (the best course of action is to send an \"issue\" in github)")
    #TODO: resolve the cases where "new new fields" appear due to differences in the ref_field only 
  }
# Putting fields in the format
  ins_fie_for_que <- "INSERT INTO spec.in_rel_field
  SELECT id_for, id_field_db
  FROM spec.in_format f , spec.tmp_corres_fie tcf
  WHERE formatname=$1 AND version=$2
  "
  res <- dbSendQuery(conn_adm, ins_fie_for_que, params = list(spec$format$formatname,spec$format$version))
  dbClearResult(res)
# Inserting new rules
  if ("rules" %in% names(spec))
  {
    if (nrow(spec$rules) > 0)
    {
      rule_query <- 
       "SELECT r.id_rule IS NULL AS \"new\", r.id_rule id_rule_db, tr.id_rule,
          typerule, cd_tab, rule, tr.comment, tr.comment IS NOT NULL AND NOT tr.comment = r.comment AS upd
       FROM spec.tmp_rules tr
       LEFT JOIN spec.in_rule r USING (typerule,cd_tab,rule)
       "
      ins_rul_que <- paste("WITH new_old AS (",rule_query,")
                           INSERT INTO spec.in_rule(typerule,cd_tab,rule,comment)
                           SELECT typerule,cd_tab,rule,comment
                           FROM new_old n
                           WHERE new
                           ORDER BY n.id_rule
                           RETURNING id_rule
                           ")
      res <- dbSendQuery(conn_adm, ins_rul_que)
      id_newRule <- dbFetch(res,-1)
      dbClearResult(res)
      message(length(id_newRule$id_rule)," new rules inserted")
      upd_rul_que <- paste("WITH new_old AS(",rule_query,")
                           UPDATE spec.in_rule r
                           SET comment=no.comment
                           FROM new_old no
                           WHERE no.upd AND no.id_rule_db=r.id_rule
                           ")
      res <- dbSendQuery(conn_adm,upd_rul_que)
      message(dbGetRowsAffected(res)," rule(s) updated")
      dbClearResult(res)
      get_rule <- dbGetQuery(conn_adm, rule_query)
      if (any(get_rule$new))
      {
        dbRollback(conn_adm)
        stop("Something went wrong with rule insertions and/or modifications, database rolling back to previous state")
      }
# Putting fields in the format
      ins_rul_for_que <- paste( "WITH new_old AS (",rule_query,")
      INSERT INTO spec.in_rel_rule
      SELECT id_for, no.id_rule_db
      FROM spec.in_format f , new_old no
      WHERE formatname=$1 AND version=$2
      ")
      res <- dbSendQuery(conn_adm, ins_rul_for_que, params = list(spec$format$formatname,spec$format$version))
      dbClearResult(res)
    }else{message("The format includes no rule")}
  }else{message("The format includes no rule")}
  if ("requirements" %in% names(spec))
  {
    if(nrow(spec$requirements) > 0)
    {
      #TODO manage requirement for input format
      stop("Requirements are not yet implemented for input formats")
    }else{message("The format includes no requirement")}
  }else{message("The format includes no requirement")}
  lapply(dbGetQuery(sib_adm, "SELECT table_name FROM information_schema.tables WHERE table_schema='spec' AND table_name ~ '^tmp_'")$table_name,
       function(x,co) dbRemoveTable(conn = co, c("spec",x)), co = sib_adm)
  dbCommit(conn_adm)
  message("New input format insertion successful (",spec$format$formatname,", version: ",spec$format$version,")")
  return(T)
}
##### END ##########

#require(RPostgreSQL)
#sib_adm <- dbConnect(PostgreSQL(), dbname = "sib_plot", user = "sib_adm")
#source("../functions/extract_function.R")
#eval(parse(text = extractFunction("../functions/specs.R", "test_regex_multi")))
#eval(parse(text = extractFunction("../functions/specs.R", "checkNewFormat")))
#eval(parse(text = extractFunction("../functions/specs.R","prepareQueryId")))
#eval(parse(text = extractFunction("../functions/specs.R","insertNewInFormat")))
#
#conn_adm <- sib_adm
#spec <- checkNewFormat(conn_adm = sib_adm, mainFile = "../inputSpec/Bst_csv/BST_csv_0.2_format.csv", typeFile = "csv")
##lapply(dbGetQuery(sib_adm, "SELECT table_name FROM information_schema.tables WHERE table_schema='spec' AND table_name ~ '^tmp_'")$table_name,
##       function(x,co) dbRemoveTable(conn = co, c("spec",x)), co = sib_adm)
#insertNewInFormat(spec,sib_adm)
