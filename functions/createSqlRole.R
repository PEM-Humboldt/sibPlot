# I am not sure it is safe again injection attack, but I did not find a way to make it completely safe yet since the parameters are not quoted
create_sib_role<- function(adm_con, rolName="sib_user", passwd=NA, superUser= F, login=!is.na(passwd), createDb= F, createRole= F, inGroup="sib")
{
  existingRoles<- dbGetQuery(adm_con,"SELECT rolname FROM pg_catalog.pg_roles")$rolname
  if(rolName %in% existingRoles)
  {
    warning(rolName, "already exists as a role in the cluster, the statement is bloqued before going to postgreSQL. If what you want is to modify an existing role, please go directly to the postgres server with administrative rights.")
    return(NULL)
  }
  if(length(inGroup)>1)
  {stop("Attributing more than one group is not supported for now")}
  noGroup<- is.na(inGroup)
  log<- login    
  superUser<- SQL(ifelse(superUser,"SUPERUSER","NOSUPERUSER"))
  createDb<- SQL(ifelse(createDb,"CREATEDB","NOCREATEDB"))
  createRole<- SQL(ifelse(createRole, "CREATEROLE", "NOCREATEROLE"))
  login<- SQL(ifelse(login, "LOGIN", "NOLOGIN"))
  if(log){
    passwd<- dbQuoteString(adm_con, passwd)
  }
  (que<- paste0("CREATE ROLE ",rolName,
    " WITH ",paste(superUser, createDb, createRole, login), 
    ifelse(log,paste0(" PASSWORD " ,passwd),""),
    ifelse(noGroup,"",paste0(" IN GROUP " ,inGroup, " INHERIT"))
  ))
  if(grepl(";",que)|grepl("--",que)){stop("SQL injection attack... Why are you doing that?")}
  return(dbSendStatement(adm_con,que))
}
