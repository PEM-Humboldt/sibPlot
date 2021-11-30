
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
test_regex_multi<-function(vecChar,regex_spec,...)
{
  mat_res<-matrix(NA,nrow=length(vecChar),ncol=sum(sapply(regex_spec,length)))
  counter<-0
  ok<-rep(names(regex_spec),sapply(regex_spec,length))
  nb<-Reduce(c,lapply(regex_spec,function(x)1:length(x)))
  for(i in names(regex_spec))
  {for(j in 1:length(regex_spec[[i]]))
  {
    counter<-counter+1
    mat_res[,counter]<-grepl(regex_spec[[i]][j],vecChar,...)
  }
  }
  if(any(rowSums(mat_res)>1))
  {
    # to do : adapt stop message (the "paste" does not work yet)
    #st_more1<-which(rowSums(mat_res)>1)
    #tabError<-paste(vecChar[st_more1],":",paste(ok[which(mat_res[st_more1])],nb[which(mat_res[st_more1])],sep=" ",collapse = " "),collapse="\n")
    #stop(paste("More than one regex was found true for the following strings:\n",tabError))
    stop("More than one regex corresponded for one of the string")
  }else{
    got<- apply(mat_res,1,function(x)ifelse(sum(x)==0,NA,which(x)))
    return(data.frame(testedString=as.character(vecChar),ok=ok[got],nb=nb[got]))
  }
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

maxVersion <- function(versions)
{
  if(length(versions)==1){return(versions)}
  spl <- lapply(strsplit(versions,"\\."),as.integer)
  max_lev <- max(sapply(spl,length))
  tabVersion <- as.data.frame(Reduce(rbind,lapply(spl, function(x,ml) c(x, rep(0,ml - length(x))),ml = max_lev)))
  ORD <- do.call(order, tabVersion)
  return(versions[which.max(ORD)])
}

listInputFormat <- function(conn_adm)
{
  dbGetQuery(conn_adm,"SELECT * FROM spec.in_format")
}

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
      "SELECT fi.id_field, fi.cd_tab, tb.tablename, fi.example, fi.regex_reco, fi.typeof, fi.unit, fi.max_char, fi.min_num, fi.max_num, fi.mandatory, fi.extra, fi.regex_field, fi.ref_table, fi.ref_field, fi.comment
      FROM spec.in_format fo
        JOIN spec.in_rel_field irt ON fo.id_for=irt.cd_for
        JOIN spec.in_fields fi ON irt.cd_field=fi.id_field
        JOIN spec.in_tables tb ON fi.cd_tab=tb.id_tab
      WHERE formatname=$1 AND version=$2",
      params = list(formatName, formatVersion))
  res$rules <- dbGetQuery(conn_adm,
      "SELECT ru.id_rule, ru.type_rule, ru.cd_tab, tb.tablename, ru.rule, ru.comment
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
  return(res)
}
conn_adm <- sib_adm
formatName <- listInputFormat(conn_adm)[1,"formatname"]
formatVersion <- "last"
getInputSpec(sib_adm,formatName)


