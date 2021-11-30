CREATE SCHEMA IF NOT EXISTS spec AUTHORIZATION sib_adm;

CREATE TABLE spec.in_tables
(
  id_tab serial PRIMARY KEY,
  tablename text NOT NULL,
  mandatory boolean DEFAULT false,
  regex_reco text,
  extra boolean DEFAULT false,
  comment text,
  UNIQUE(tablename,mandatory,regex_reco)
);


CREATE TABLE spec.def_typeof
(
  id_to varchar(50) PRIMARY KEY,
  type_postgres varchar(50),
  type_sqlite varchar(50),
  type_r varchar(50),
  description text
);

CREATE TABLE spec.in_fields
(
  id_field serial PRIMARY KEY,
  cd_tab INTEGER REFERENCES spec.in_tables(id_tab) NOT NULL,
  fieldname text NOT NULL,
  example text,
  regex_reco text,
  typeof varchar REFERENCES spec.def_typeof(id_to) NOT NULL,
  unit varchar(10),
  max_char integer,
  min_num double precision,
  max_num double precision,
  mandatory boolean DEFAULT false NOT NULL,
  extra boolean DEFAULT false NOT NULL,
  regex_field text,
  ref_table integer REFERENCES spec.in_tables(id_tab),
  ref_field integer REFERENCES spec.in_fields(id_field),
  comment text,
  CHECK (fieldname !~ ';' AND fieldname !~ '--'),
  CHECK (NOT (typeof = 'varchar' AND max_char IS NULL))
);

CREATE TABLE spec.def_rule
(
  id_typerule varchar(50) PRIMARY KEY,
  description_typerule text
);


CREATE TABLE spec.in_rule
(
  id_rule serial PRIMARY KEY,
  type_rule varchar(50) REFERENCES spec.def_rule(id_typerule),
  cd_tab integer REFERENCES spec.in_tables(id_tab),
  rule text,
  comment text,
  CHECK (rule !~ ';' AND rule !~ '--' )
);


CREATE TABLE spec.in_format
(
  id_for serial PRIMARY KEY,
  formatname text,
  version varchar(10),
  created_by text,
  creation_date TIMESTAMP,
  install_date TIMESTAMP,
  description text,
  CHECK (version ~ '[0-9]{1,3}\.[0-9]{1,3}'),
  UNIQUE (formatname,version)
);


CREATE TABLE spec.def_type_requi
(
  id_tr varchar(50) PRIMARY KEY,
  desc_tr text
);

CREATE TABLE spec.in_requi
(
  cd_for integer REFERENCES spec.in_format(id_for),
  cd_tr varchar(50) REFERENCES spec.def_type_requi(id_tr),
  requirement text
);

CREATE TABLE spec.in_rel_tab
(
  cd_for integer REFERENCES spec.in_format(id_for),
  cd_tab integer REFERENCES spec.in_tables(id_tab)
);

CREATE TABLE spec.in_rel_field
(
  cd_for integer REFERENCES spec.in_format(id_for),
  cd_field integer REFERENCES spec.in_fields(id_field)
);

CREATE TABLE spec.in_rel_rule
(
  cd_for integer REFERENCES spec.in_format(id_for),
  cd_rule integer REFERENCES spec.in_rule(id_rule)
);


INSERT INTO spec.def_typeof VALUES
('serial','SERIAL','INTEGER AUTOINCREMENT','integer','Integer which autoincrements himself, regularly used for creating integer ids'),
('varchar','VARCHAR','TEXT','character','strings with a limited number of characters'),
('text','TEXT','TEXT','character','character strings (unlimited number of characters)'),
('integer','INTEGER','INTEGER','integer','numeric type: integer'),
('double precision','DOUBLE PRECISION','REAL','double','numeric type used to store precise numbers'),
('date','DATE','TEXT','Date','date (note that this type does not exist for SQLite)'),
('time','TIME WITHOUT TIMEZONE','TEXT','times','time type, note that in R, it is a class defined in the chron package')
;

INSERT INTO spec.def_rule VALUES
('UNIQUE','unique rule applied on more than one column'),
('CHECK','boolean which need to be true in order to be accepted in the data table');
;
