dbHasSuperuser<- function(con){
  user<- dbGetInfo(con)$user
  sus<- dbGetQuery(con, "SELECT rolname FROM pg_catalog.pg_roles WHERE rolsuper")$rolname
  return(user%in%sus)
}
dbHasCreateDb<- function(con){
  user<- dbGetInfo(con)$user
  cds<- dbGetQuery(con, "SELECT rolname FROM pg_catalog.pg_roles WHERE rolcreatedb")$rolname
  return(user%in%cds)
}
dbHasCreateRole<- function(con){
  user<- dbGetInfo(con)$user
  crs<- dbGetQuery(con, "SELECT rolname FROM pg_catalog.pg_roles WHERE rolcreaterole")$rolname
  return(user%in%crs)
}