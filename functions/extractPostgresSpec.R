# To do, I passed by information_schema because it does not use the internal identifiers but names of the relations, but it seems safer to pass by other tables, like the one used by the metacommand of postgreSQL.
# In particular, in order to find the referenced columns, this method find the unique constraint related to the foreign key, which is fine in simple cases but might not be sufficient in more complex cases

dbGetColumnSpecs <- function(con,schema,table)
{
 query <- "WITH 
single_col_constraints AS(
    SELECT 
        constraint_name,
        count(DISTINCT column_name) 
    FROM information_schema.key_column_usage
    WHERE 
        table_schema = $1 AND
        table_name = $2
    GROUP BY constraint_name 
    HAVING count(DISTINCT column_name)=1
), pk AS(
    SELECT kcu.table_schema, kcu.table_name, kcu.column_name
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu ON tc.constraint_name=kcu.constraint_name
    WHERE constraint_type = 'PRIMARY KEY'
), fk_simple AS(
    SELECT kcu.table_schema, kcu.table_name, kcu.column_name, 
        kcu2.table_schema || '.' || kcu2.table_name || '(' || kcu2.column_name  || ')' AS referenced 
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu ON tc.constraint_name=kcu.constraint_name
    JOIN information_schema.referential_constraints rc ON tc.constraint_name=rc.constraint_name
    JOIN information_schema.key_column_usage kcu2 ON rc.unique_constraint_name=kcu2.constraint_name
    WHERE tc.constraint_type = 'FOREIGN KEY' AND tc.constraint_name IN (SELECT constraint_name FROM single_col_constraints)
), c_un AS(
    SELECT kcu.table_schema, kcu.table_name,kcu.column_name
    FROM single_col_constraints scc
        JOIN information_schema.table_constraints tc ON scc.constraint_name=tc.constraint_name
        JOIN information_schema.key_column_usage kcu ON scc.constraint_name=kcu.constraint_name
    WHERE constraint_type IN ('UNIQUE','PRIMARY KEY')
)
SELECT 
  ic.table_schema AS \"schema\",
  ic.table_name AS \"table\",
  ic.column_name AS \"column\",
  NOT ic.is_nullable::boolean AS \"mandatory\",
  CASE
    WHEN ic.data_type = 'integer' AND pg_get_serial_sequence(ic.table_schema||'.'||ic.table_name,ic.column_name::text) IS NOT NULL THEN 'serial'
    ELSE ic.data_type
  END AS data_type,
  character_maximum_length AS max_char,
  pk.column_name IS NOT NULL AS pk,
  c_un.column_name IS NOT NULL AS unique,
  fk_simple.referenced AS foreign_key
FROM
  information_schema.columns ic
  LEFT JOIN pk ON ic.table_schema=pk.table_schema AND ic.table_name=pk.table_name AND ic.column_name=pk.column_name
  LEFT JOIN fk_simple ON ic.table_schema=fk_simple.table_schema AND ic.table_name=fk_simple.table_name AND ic.column_name=fk_simple.column_name
  LEFT JOIN c_un ON ic.table_schema=c_un.table_schema AND ic.table_name=c_un.table_name AND ic.column_name=c_un.column_name
WHERE 
  ic.table_schema = $1
  AND ic.table_name = $2
ORDER BY ic.ordinal_position
"
res <- dbSendQuery(con, query, params = list(schema,table))
r <- dbFetch(res)
dbClearResult(res)
return(r)
}

dbOtherTableConstraints <- function(con,schema,table)
{
query <- "WITH 
  multi_col_constraints AS(
    SELECT 
    constraint_name,
    count(DISTINCT column_name) 
    FROM information_schema.key_column_usage
    WHERE 
    table_schema = $1 AND
    table_name = $2
    GROUP BY constraint_name 
    HAVING count(DISTINCT column_name)>1
  )
  SELECT tc.table_schema \"schema\", tc.table_name \"table\",mcc.constraint_name, constraint_type, '(' || ARRAY_TO_STRING(ARRAY_AGG(kcu.column_name),',') || ')' cols
  FROM multi_col_constraints mcc
  LEFT JOIN information_schema.table_constraints tc ON mcc.constraint_name=tc.constraint_name
  LEFT JOIN information_schema.key_column_usage kcu ON mcc.constraint_name=kcu.constraint_name
  GROUP BY tc.table_schema,tc.table_name,mcc.constraint_name, constraint_type
  UNION ALL
  SELECT table_schema \"schema\", table_name \"table\",constraint_name, constraint_type, NULL
  FROM information_schema.table_constraints
  WHERE table_schema=$1 AND table_name=$2 AND
  NOT constraint_name IN (SELECT constraint_name FROM multi_col_constraints) AND
  NOT (constraint_type = 'CHECK' AND constraint_name ~ 'not_null') AND
  NOT (constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY')) AND
  NOT (constraint_type = 'UNIQUE')
  "
res <- dbSendQuery(con, query, params = list(schema,table))
r <- dbFetch(res)
dbClearResult(res)
return(r)
  
}
