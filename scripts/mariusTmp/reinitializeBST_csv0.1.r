setwd("/home/marius/Travail/traitementDonnees/2021_Humboldt_Sib/sibPlot/devDocs/")

folSpec <- "../inputSpec/permanentBstCsv/"
A <- lapply(paste0(folSpec,dir(folSpec, pattern = "\\.csv$")),read.csv)
A[[1]]$typeof[A[[1]]$typeof=="character"&!is.na(A[[1]]$maxChar)]<- "varchar"
A[[1]]$typeof[A[[1]]$typeof=="character"&is.na(A[[1]]$maxChar)]<- "text"
A[[1]]$typeof[A[[1]]$typeof=="double"]<- "double precision"
A[[1]]$table
A[[1]]$ordercol <- numeric(nrow(A[[1]]))
for(i in 1:nrow(A[[1]]))
{
  counter <- max(A[[1]]$ordercol[A[[1]]$table==A[[1]][i,"table"]])+1
  A[[1]][i,"ordercol"] <- counter
  
}

require(RPostgreSQL)
sib_adm <- dbConnect(PostgreSQL(), dbname = "sib_plot", user = "sib_adm", password = "sib")
dbBegin(sib_adm)
dbWriteTable(conn = sib_adm,name = c("spec","tmp_table"),value = A[[2]], temporary = T)
#dbReadTable(sib_adm,c("spec","tmp_table"))
tabs <- dbSendQuery(sib_adm,
                    "INSERT INTO spec.in_tables(tablename,mandatory,regex_reco)
SELECT tablename, mandatory, regex_reco
FROM spec.tmp_table
"
)
nb_tabs <- dbGetRowsAffected(tabs)
maxIdTab <- dbGetQuery(sib_adm,"SELECT max(id_tab) nb FROM spec.in_tables")$nb
lineMinTab <- maxIdTab - nb_tabs
dbClearResult(tabs)
dbWriteTable(sib_adm, name = c("spec", "tmp_fields"), value = A[[1]][,!apply(A[[1]],2,function(x)all(is.na(x)))], temporary = T)
que <- "
WITH with_tabcd AS (
SELECT t.id_tab,f.*
FROM spec.tmp_fields f
LEFT JOIN spec.in_tables t ON f.table = t.tablename AND t.id_tab > $1
)
INSERT INTO spec.in_fields(cd_tab,fieldname,ordercol,example,regex_reco,typeof,max_char,mandatory,extra,regex_field,comment)
(SELECT id_tab, field, ordercol, example, \"recoField\", typeof,\"maxChar\",mandatory,false,regex,NULL
FROM with_tabcd
ORDER BY id_tab, ordercol)
"
#cat(sub("\\$1",lineMinTab,que))
fie <- dbSendQuery(sib_adm, que, params = list(lineMinTab))
nbFie <- dbGetRowsAffected(fie)
ins_for <- dbSendQuery(sib_adm,"INSERT INTO spec.in_format(formatname,version,created_by,creation_date,install_date,description)
            VALUES('BST_csv','0.1','marius',NOW(),NOW(),'Format for the \"red BST-COL\" consisting of a group of csv files')
            ")
dbClearResult(ins_for)
dbClearResult(fie)
ins_rel_tab <- dbSendQuery(sib_adm,
                           "INSERT INTO spec.in_rel_tab
            SELECT fo.id_for,ta.id_tab
            FROM 
              spec.in_format fo,
              spec.in_tables ta
            WHERE fo.id_for= (SELECT max(id_for) FROM spec.in_format)
              AND
                ta.id_tab>$1
            ",params = list(lineMinTab))
dbClearResult(ins_rel_tab)
ins_rel_fie <- dbSendQuery(sib_adm,"INSERT INTO spec.in_rel_field
            SELECT fo.id_for,fi.id_field
            FROM
              spec.in_format fo,
              spec.in_fields fi
            WHERE fo.id_for= (SELECT max(id_for) FROM spec.in_format)
              AND
                fi.id_field>$1
            ",params=list(dbGetQuery(sib_adm,"SELECT max(id_field) nb FROM spec.in_fields")$nb - nbFie))
dbClearResult(ins_rel_fie)
dbRemoveTable(sib_adm,c("spec","tmp_table"))
dbRemoveTable(sib_adm,c("spec","tmp_fields"))
dbCommit(sib_adm)
dbDisconnect(sib_adm)