sib_create_tablespace<- function(adm_con,nameTableSpace,path){
  if(!dbHasSuperuser(adm_con))
  {stop("You need to do that as a superuser!")}
  if(nameTableSpace%in%dbGetQuery(adm_con, "SELECT spcname FROM pg_catalog.pg_tablespace")$spcname)
  {
    warning(nameTableSpace," exists already! Bloqued before going to postgres")
    return(NULL)
  }
  if(!file.exists(path))
  {dir.create(path, recursive = T)}
  if(grepl(";",nameTableSpace)|grepl("--",nameTableSpace)){
    stop("SQL injection attack... Why are you doing that?")
  }
  path<-dbQuoteString(adm_con,path)
  que<- paste0("CREATE TABLESPACE ", nameTableSpace, " LOCATION ",path)
  return(dbSendStatement(adm_con,que))
}


sib_create_database<- function(adm_con, nameDb, owner="sib", encoding="UTF8", nameTableSpace=NA, template= "template0", lc_ctype = "en_US.UTF-8", lc_collate = 'en_US.UTF-8')
{
  if(!dbHasCreateDb(adm_con)){
    stop("You need the 'CREATEDB' attribute to do that")
  }
  if(nameDb %in% dbGetQuery(adm_con, "SELECT datname FROM pg_catalog.pg_database")$datname)
  {
    warning(nameDb," exists already! Bloqued before going to postgres")
    return(NULL)
  }
  if(grepl(";",nameTableSpace)|grepl("--",nameTableSpace)|
     grepl(";",nameDb)|grepl("--",nameDb)|
     grepl(";",template)|grepl("--",template)|
     grepl(";",lc_ctype)|grepl("--",lc_ctype)|
     grepl(";",lc_collate)|grepl("--",lc_collate)
     )
  {
    stop("SQL injection attack... Why are you doing that?")
  }
  if(!is.na(nameTableSpace))
  {
    rightsTableSpace<-dbGetQuery(adm_con,"SELECT spcname, spcacl FROM pg_catalog.pg_tablespace")
    rightsInt<- rightsTableSpace[rightsTableSpace$spcname == nameTableSpace,"spcacl"]
    rightsInt<- gsub("[{}]","",rightsInt)
    sep<- strsplit(rightsInt,",")[[1]]
    usersAuth<- sub("^(.+)=.+$","\\1",sep)
    ok<- dbGetInfo(adm_con)$user %in% usersAuth
    if(!ok)
    {stop("You do not have postgres permissions on the \'",nameTableSpace,"\' tableSpace")}
  }  
  que<- paste("CREATE DATABASE",nameDb,
               "OWNER",owner,
               ifelse(is.na(encoding),"",paste("ENCODING",dbQuoteString(adm_con,encoding))),
               ifelse(is.na(nameTableSpace),"",paste("TABLESPACE",nameTableSpace)),
               ifelse(is.na(template),"",paste("TEMPLATE",template)),
               ifelse(is.na(lc_ctype),"",paste("LC_CTYPE",dbQuoteString(adm_con,lc_ctype))),
               ifelse(is.na(lc_collate),"",paste("LC_COLLATE",dbQuoteString(adm_con,lc_collate)))
  )
  #return(que)
  return(dbSendStatement(adm_con,que))
}