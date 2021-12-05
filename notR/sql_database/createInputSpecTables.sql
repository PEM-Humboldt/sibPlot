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

CREATE TABLE spec.def_lev_ref
(
  id_lev_ref varchar(50) PRIMARY KEY,
  description text,
  ref_pk text
);

CREATE TABLE spec.in_fields
(
  id_field serial PRIMARY KEY,
  cd_tab INTEGER REFERENCES spec.in_tables(id_tab) NOT NULL,
  fieldname text NOT NULL,
  ordercol integer,
  example text,
  regex_reco text,
  typeof varchar REFERENCES spec.def_typeof(id_to) NOT NULL,
  unit varchar(10),
  max_char integer,
  min_num double precision,
  max_num double precision,
  mandatory boolean DEFAULT false NOT NULL,
  extra boolean DEFAULT false NOT NULL,
  lev_ref varchar(50) REFERENCES spec.def_lev_ref(id_lev_ref),
  regex_field text,
  ref_table integer REFERENCES spec.in_tables(id_tab),
  ref_field integer REFERENCES spec.in_fields(id_field),
  comment text,
  CHECK (fieldname !~ ';' AND fieldname !~ '--'),
  CHECK (NOT (typeof = 'varchar' AND max_char IS NULL)),
  UNIQUE(cd_tab, fieldname, regex_reco, typeof, unit, max_char, min_num, max_num, mandatory, extra, regex_field, ref_table, ref_field),
  UNIQUE(cd_tab,ordercol)
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
  CHECK (rule !~ ';' AND rule !~ '--' ),
  UNIQUE (type_rule, cd_tab, rule, comment)
);

CREATE TABLE spec.def_in_steps
(
  id_step varchar(50) PRIMARY KEY,
  step_description text
);

CREATE TABLE spec.in_functions
(
  id_func serial PRIMARY KEY,
  cd_step varchar(50) REFERENCES spec.def_in_steps(id_step),
  function_r varchar(250),
  args text,
  variable_res varchar(250)
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
  cd_for integer REFERENCES spec.in_format(id_for) ON DELETE CASCADE ON UPDATE CASCADE,
  cd_tr varchar(50) REFERENCES spec.def_type_requi(id_tr) ON DELETE CASCADE ON UPDATE CASCADE,
  requirement text
);

CREATE TABLE spec.in_rel_tab
(
  cd_for integer REFERENCES spec.in_format(id_for) ON DELETE CASCADE ON UPDATE CASCADE,
  cd_tab integer REFERENCES spec.in_tables(id_tab) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE spec.in_rel_field
(
  cd_for integer REFERENCES spec.in_format(id_for) ON DELETE CASCADE ON UPDATE CASCADE,
  cd_field integer REFERENCES spec.in_fields(id_field) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE spec.in_rel_rule
(
  cd_for integer REFERENCES spec.in_format(id_for) ON DELETE CASCADE ON UPDATE CASCADE,
  cd_rule integer REFERENCES spec.in_rule(id_rule) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE spec.in_rel_func
(
  cd_for integer REFERENCES spec.in_format(id_for) ON DELETE CASCADE ON UPDATE CASCADE,
  cd_func integer REFERENCES spec.in_functions(id_func) ON DELETE CASCADE ON UPDATE CASCADE
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
('CHECK','boolean which need to be true in order to be accepted in the data table'),
('ALL_IDENTICAL','expression which forces all values to be identical depending on another value example: "field1 IN field2, field3" note that field may be identified by their cd_field (recommended)'),
('FOREIGN_MULTI','Multiple foreign key, the association of value must exist in the referenced table example "tab1(field1,field2) REFERENCES tab2(field3,field4)" note that fields and tables must be identified by their cd_field and cd_tab')
;

INSERT INTO spec.def_lev_ref VALUES
('tag','references a particular ramet of an individual',NULL),
('ind','references a particular individual (which may have various ramet)',NULL),
('samp_unit','references a particular sampling unit (subplot or particular sampling effort made in a plot',NULL),
('plot','references a particular plot',NULL),
('denom','references a particular taxonomic denomination',NULL),
('tax','references a particular taxonomic name (accepted, clean name)',NULL),
('pers','references a particular person',NULL),
('partic','references a particular participation of a person in a project',NULL),
('event','references a particular event (field, laboratory, or input)',NULL)
;

INSERT INTO spec.def_in_steps VALUES
('reading_files','reading files from a directory and put tables in a list'),
('preformatting','preformatting the tables'),
('separ_reco','recognizing the tables'),
('separ_input','separating the input cd in the table list'),
('check_integrity_input','checking the integrity of data in the input list'),
('changes_spec','recognizing the potential changes to make in the input format and integrating them'),
('compar_existing','Comparing the input data with the existing ones in order to determine the potential errors and preparing the way to insert new data'),
('transf_input','transforming the input data in order to make it easier to insert into the database'),
('insert','inserting the new data in the database');
