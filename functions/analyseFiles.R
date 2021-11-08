## ---- setup-----------------------------------------------------------------------------------------------------------------------------------------
require(knitr)&require(RPostgreSQL)&require(formatR)&require(kableExtra)
opts_chunk$set(cache=F,fig.path="./Fig/",tidy='styler',cache.rebuild = F,formatSQL = TRUE)
#fileFunctions<-dir("../sib_plot/functions/",pattern=".*\\.R")
#sapply(fileFunctions,function(x)source(paste("../sib_plot/functions/",x,sep="")))


## ---------------------------------------------------------------------------------------------------------------------------------------------------
purl("readingFile.Rmd")
source("readingFile.R")


## ---- tidy=TRUE,tidy.opts=list(width.cutoff=70)-----------------------------------------------------------------------------------------------------
roySpec<-xl_sep_table("../../AnalysisDocumentosRoy/RelacionalDAta.xlsx" , sheet="Sheet1")
names(roySpec)<-sapply(roySpec , function(x) sub("_plotName$","" , colnames(x)[1]))
for(i in 1:length(roySpec)) { colnames(roySpec[[i]])[1] <- "field"}
tabRoySpec <- data.frame(table = rep(names(roySpec) , sapply(roySpec , nrow)) , Reduce(rbind , roySpec))
table(tabRoySpec$optimalCharacters)
tabRoySpec$typeof<-NA
tabRoySpec$typeof[tabRoySpec$optimalCharacters=="integer"]<-"integer"
tabRoySpec$typeof[tabRoySpec$optimalCharacters=="numeric"]<-"double"
tabRoySpec$typeof[grep("character",tabRoySpec$optimalCharacters)]<-"character"
tabRoySpec$typeof[tabRoySpec$field=="tag"]<-"character"
tabRoySpec$maxChar<-NA
regexMax<-".*[-–](max)?([0-9]{1,2}).*"
tabRoySpec$maxChar[grep(regexMax , tabRoySpec$optimalCharacters)]<-as.integer(sub(regexMax , "\\2" , tabRoySpec$optimalCharacters[grep(regexMax , tabRoySpec$optimalCharacters)]))
tabRoySpec$regex <- tabRoySpec$maxNum <- tabRoySpec$minNum <- NA
tabRoySpec[ , c("table" , "field" , "example" , "typeof" , "maxChar" , "regex" , "maxNum" , "minNum")]  %>%
  head(20) %>% kable(booktab=T,caption="Specification of the fields (20 first lines)") %>% kable_styling(font_size=8)
tabRoySpec_Table<-data.frame(typeTable=unique(tabRoySpec$table),regex=paste0("^",unique(tabRoySpec$table),"$"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------
dosSpec<-"../inputSpec/"
if(!file.exists(dosSpec))
{dir.create(dosSpec)}
#wb<-createWorkbook()
#addWorksheet(wb,sheetName="tableSpec")
#addWorksheet(wb,sheetName="fieldSpec")
#writeDataTable(wb,sheet=1,x=tabRoySpec_Table,colNames = T , rowNames = F )
#writeDataTable(wb,sheet=2,x=tabRoySpec,colNames = T , rowNames = F )
#setColWidths(wb,1,cols=1:ncol(tabRoySpec_Table),"auto")
#setColWidths(wb,2,cols=1:ncol(tabRoySpec),"auto")
#saveWorkbook(wb,paste0(dosSpec,"/","spec_csvPermanentBST_Roy.xlsx"),overwrite=T)
#if(!"spec_csvPermanentBST_Final.xlsx"%in%dir(dosSpec))
#{saveWorkbook(wb,paste0(dosSpec,"/","spec_csvPermanentBST_Final.xlsx"))}



## ---------------------------------------------------------------------------------------------------------------------------------------------------
read_Xlspec<-function(file_spec)
{
  tb_spec<-openxlsx::read.xlsx(file_spec,sheet = "tableSpec")
  regex_tb<-strsplit(tb_spec$regex,";")
  names(regex_tb)<-tb_spec$typeTable
  fi_spec<-openxlsx::read.xlsx(file_spec,sheet = "fieldSpec")
  regex_fi<-strsplit(fi_spec$regex[!is.na(fi_spec$regex)],";")
  names(regex_fi)<-fi_spec$field[!is.na(fi_spec$regex)]
  fi_spec<-fi_spec[,c("table","field","example","typeof","maxChar","minNum","maxNum")]
  if(any(!fi_spec$table%in%tb_spec$typeTable))
  {
    stop(paste("In the field specs we found the following tables:", paste(unique(fi_spec$table[which(!fi_spec$table%in%tb_spec$typeTable)]),collapse=", "),"which ARE NOT DEFINED in the table specs",sep="\n"))
  }
  return(list(regex_tb=regex_tb,fi_spec=fi_spec,regex_fi=regex_fi))
}
read_CsvSpec<-function(fileSpecTab, fileSpecField, fileSpecExtra=NA, fileSpecPrefor=NA)
{
  tb_spec<-read.csv(fileSpecTab)
  regex_tb<-strsplit(tb_spec$regex,";")
  names(regex_tb)<-tb_spec$typeTable
  fi_spec<-read.csv(fileSpecField)
  recoField<-strsplit(fi_spec$recoField[!is.na(fi_spec$recoField)&!fi_spec$recoField==""],";")
  names(recoField)<-fi_spec$field[!is.na(fi_spec$recoField)&!fi_spec$recoField==""]
  regex_fi<-strsplit(fi_spec$regex[!is.na(fi_spec$regex)&!fi_spec$regex==""],";")
  names(regex_fi)<-fi_spec$field[!is.na(fi_spec$regex)&!fi_spec$regex==""]
  fi_spec<-fi_spec[,c("table","field","recoField","mandatory","example","typeof","maxChar","minNum","maxNum","regex","includedFormat")]
  if(any(!fi_spec$table%in%tb_spec$typeTable))
  {
    stop(paste("In the field specs we found the following tables:", paste(unique(fi_spec$table[which(!fi_spec$table%in%tb_spec$typeTable)]),collapse=", "),"which ARE NOT DEFINED in the table specs",sep="\n"))
  }
  
  pf_spec<-NULL
  if(!is.na(fileSpecPrefor))
  {
    tb_pf <- read.csv(fileSpecPrefor)
    recoField_pf<- strsplit(tb_pf$recoField[!is.na(tb_pf$recoField)&!tb_pf$recoField==""],";")
    names(recoField_pf)<- tb_pf$field[!is.na(tb_pf$recoField)&!tb_pf$recoField==""]
    pf_spec<- list(tb_pf=tb_pf, recoField_pf= recoField_pf)
  }
  return(list(regex_tb= regex_tb, fi_spec= fi_spec, recoField= recoField, regex_fi= regex_fi, pf_spec=pf_spec))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------
#specs<-read_Xlspec(paste0(dosSpec,"/","spec_csvPermanentBST_Final.xlsx"))
specs<- read_CsvSpec(fileSpecTab = paste0(dosSpec,"/","spec_csvPermanentBST_tables.csv"),
                     fileSpecField = paste0(dosSpec,"/","spec_csvPermanentBST_fields.csv")
                     )


## ---------------------------------------------------------------------------------------------------------------------------------------------------
# Note: regex spec is a list of 2 levels
# The names of the first level gives the resulting results (which is the field or table which is compatible)
# the index of the second level give the specific regex which resulted true
# The function checks whether there is only one regex being true (error if not)
# and then send a table with the regex which works if any and the index in the regex group
test_regex_multi<-function(vecChar,regex_spec)
{
  mat_res<-matrix(NA,nrow=length(vecChar),ncol=sum(sapply(regex_spec,length)))
  counter<-0
  ok<-rep(names(regex_spec),sapply(regex_spec,length))
  nb<-Reduce(c,lapply(regex_spec,function(x)1:length(x)))
  for(i in names(regex_spec))
  {for(j in 1:length(regex_spec[[i]]))
    {
      counter<-counter+1
      mat_res[,counter]<-grepl(regex_spec[[i]][j],vecChar)
    }
  }
  if(any(rowSums(mat_res)>1))
  {
    # to do : adapt stop message (the "paste" does not work yet)
    #st_more1<-which(rowSums(mat_res)>1)
    #tabError<-paste(vecChar[st_more1],":",paste(ok[which(mat_res[st_more1])],nb[which(mat_res[st_more1])],sep=" ",collapse = " "),collapse="\n")
    #stop(paste("More than one regex was found true for the following strings:\n",tabError))
    stop()
  }else{
    return(data.frame(testedString=vecChar,ok=ok[apply(mat_res,1,which)],nb=nb[apply(mat_res,1,which)]))
  }
}
test_regex<-function(vecChar,vecRegex)
{
  mat_res<-matrix(nrow=length(vecChar),ncol=length(vecRegex))
  for(i in 1:length(vecRegex))
  {mat_res[,i]<-grepl(vecRegex[i],vecChar)}
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------
# Here I will take some examples to work on input data from Roy's code

# I believe it would be a good thing to have first a function which checks files, files types etc
#dos<-"~/Travail/traitementDonnees/2021_Humboldt_Sib/CodeRoy/BasesHumboldt-MV/core/metadata/"
dos<-"../../CodeRoy/BasesHumboldt-MV/core"




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

an_fi<-analyseCsvFiles(dos,regex_types=specs$regex_tb)


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

firstRead<-readCsvFiles(an_fi$file,an_fi$type_cat)

# Function to check whether the names of the fields are the same than the ones prepared in the formats
listField<-tapply(specs$fi_spec$field,specs$fi_spec$table,function(x)x)
colRead<-lapply(firstRead,colnames)
checkNamesFields<-function(read,categ,listFieldCateg)
{
  
  corresColRef<-listFieldCateg[match(categ,names(listFieldCateg))]
  res<-mapply(function(nam,ref)
    {
      #allTrueSimple <- all(nam==ref)
      list(notDefined=setdiff(nam,ref),Missing=setdiff(ref,nam))
    },colListRead,corresColRef,SIMPLIFY = F)
  return(res)
}

#C<-checkNamesFields(read = colRead, categ = an_fi$type_cat, listFieldCateg = listField)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
names(table(an_fi$plot))[table(an_fi$plot)<5]
table(an_fi$type_cat)
#Which plot do not have any census0 (which seems to be basic for me)
plot_census0<-by(an_fi,an_fi$plot,function(x)any(x$type=="census0"),simplify = T)
names(which(!plot_census0))



#Function for treating dates (todo: check whether Roy's treatment is the same everywhere)
treatDates<- function(textDate)
{
  regexDate<-"^([0-9]{1,4})/([0-9]{1,4})/([0-9]{1,4})$"
  date3numbers<-grepl(textDate,regexDate)
  if(all(date3numbers))
  {
    part1<-sub(regexDate,"\\1",texDate)
    part2<-sub(regexDate,"\\2",texDate)
    part3<-sub(regexDate,"\\3",texDate)
    part1num<-as.integer(part1)
    part2num<-as.integer(part2)
    part3num<-as.integer(part3)
    # Which are years for sure
    p1y<-any(part1num>31)
    p2y<-any(part2num>31)
    p3y<-any(part3num>31)
    if((p1y+p2y+p3y)>1){
      stop("In the dates, more than one part can only be year")
      }else{
      
    }
  }else{
    stop("Some dates are not 3 numbers separated by \"/\"")
  }
  
}


# Function to check whether the names of the fields are the same than the ones prepared in the formats
checkNamesFields<-function(x)
  NA

# Function to check whether the types entered in the fields are the same that were prepared in the formats
checkTypesFields<-function(x)
  NA
  
  

dos<-"~/Travail/traitementDonnees/2021_Humboldt_Sib/CodeRoy/BasesHumboldt-MV/core/metadata/"
dir(dos)

ex<-("AltoSanJorgeInicial")
read.csv(paste0(dos,"/metadata_",ex,".csv"))



read_metadata<-function(file)
{
  NA
}

