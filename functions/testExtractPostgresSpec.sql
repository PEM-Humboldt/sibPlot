WITH 
multi_col_constraints AS(
    SELECT 
        constraint_name,
        count(DISTINCT column_name) 
    FROM information_schema.key_column_usage
    WHERE 
        table_schema = 'main' AND
        table_name = 'tag_mortality'
    GROUP BY constraint_name 
    HAVING count(DISTINCT column_name)>1
),
single_col_constraints AS(
    SELECT 
        constraint_name,
        count(DISTINCT column_name) 
    FROM information_schema.key_column_usage
    WHERE 
        table_schema = 'main' AND
        table_name = 'tag_mortality'
    GROUP BY constraint_name 
    HAVING count(DISTINCT column_name)=1
), pk AS(
    SELECT kcu.table_schema, kcu.table_name, kcu.column_name
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu ON tc.constraint_name=kcu.constraint_name
    WHERE constraint_type = 'PRIMARY KEY'
), fk_simple AS(
    SELECT kcu.table_schema, kcu.table_name, kcu.column_name, 
        tc.table_schema || '.' || tc.table_name || '(' || kcu2.column_name  || ')' AS referenced 
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
    WHERE constraint_type = 'UNIQUE'
)
SELECT 
  ic.table_schema AS "schema",
  ic.table_name AS "table",
  ic.column_name AS "column",
  NOT ic.is_nullable::boolean AS "mandatory",
  CASE
    WHEN ic.data_type = 'integer' AND pg_get_serial_sequence(ic.table_schema||'.'||ic.table_name,ic.column_name::text) IS NOT NULL THEN 'serial'
    ELSE ic.data_type
  END AS data_type,
  character_maximum_length AS max_char,
  pk.column_name IS NOT NULL AS pk,
  c_un.column_name IS NOT NULL AS unique,
  fk_simple.referenced
FROM
  information_schema.columns ic
  LEFT JOIN pk ON ic.table_schema=pk.table_schema AND ic.table_name=pk.table_name AND ic.column_name=pk.column_name
  LEFT JOIN fk_simple ON ic.table_schema=fk_simple.table_schema AND ic.table_name=fk_simple.table_name AND ic.column_name=fk_simple.column_name
  LEFT JOIN c_un ON ic.table_schema=c_un.table_schema AND ic.table_name=c_un.table_name AND ic.column_name=c_un.column_name
WHERE 
  ic.table_schema = 'main'
  AND ic.table_name = 'tag_mortality'
ORDER BY ic.ordinal_position
;

WITH 
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
SELECT mcc.constraint_name, constraint_type, '(' || ARRAY_TO_STRING(ARRAY_AGG(kcu.column_name),',') || ')' cols
FROM multi_col_constraints mcc
    LEFT JOIN information_schema.table_constraints tc ON mcc.constraint_name=tc.constraint_name
    LEFT JOIN information_schema.key_column_usage kcu ON mcc.constraint_name=kcu.constraint_name
GROUP BY mcc.constraint_name, constraint_type
UNION ALL
SELECT constraint_name, constraint_type, NULL
FROM information_schema.table_constraints
WHERE table_schema=$1 AND table_name=$2
    NOT constraint_name IN (SELECT constraint_name FROM multi_col_constraints) AND
    NOT (constraint_type = 'CHECK' AND constraint_name ~ 'not_null') AND
    NOT (constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY')) AND
    NOT (constraint_type = 'UNIQUE')
;

-- getting constraints concerning tables
SELECT * FROM information_schema.table_constraints 

-- getting primary keys
SELECT kcu.table_schema, kcu.table_name, kcu.column_name
FROM information_schema.table_constraints tc
  JOIN information_schema.key_column_usage kcu ON tc.constraint_name=kcu.constraint_name
WHERE constraint_type = 'PRIMARY KEY';

--CONSTRAINTS CONCERNING more than a column:
SELECT 
  constraint_name,
  count(DISTINCT column_name) 
FROM information_schema.key_column_usage
GROUP BY constraint_name 
HAVING count(DISTINCT column_name)>1;

-- CONSTRAINTS CONCERNING only one column
SELECT 
constraint_name,
count(DISTINCT column_name) 
FROM information_schema.key_column_usage
GROUP BY constraint_name 
HAVING count(DISTINCT column_name)=1;



select kcu.table_schema || '.' || kcu.table_name as foreign_table,
       '>-' as rel,
       rel_kcu.table_schema || '.' || rel_kcu.table_name as primary_table,
       kcu.ordinal_position as no,
       kcu.column_name as fk_column,
       '=' as join,
       rel_kcu.column_name as pk_column,
       kcu.constraint_name
from information_schema.table_constraints tco
join information_schema.key_column_usage kcu
          on tco.constraint_schema = kcu.constraint_schema
          and tco.constraint_name = kcu.constraint_name
join information_schema.referential_constraints rco
          on tco.constraint_schema = rco.constraint_schema
          and tco.constraint_name = rco.constraint_name
join information_schema.key_column_usage rel_kcu
          on rco.unique_constraint_schema = rel_kcu.constraint_schema
          and rco.unique_constraint_name = rel_kcu.constraint_name
          and kcu.ordinal_position = rel_kcu.ordinal_position
where tco.constraint_type = 'FOREIGN KEY'
order by kcu.table_schema,
         kcu.table_name,
         kcu.ordinal_position;
