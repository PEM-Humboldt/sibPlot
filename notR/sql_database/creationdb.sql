
/*Organization schema and authorizations etc.*/



/* Creation tables 
I will start replicating exactly Roy's structure, then we will think how to modify it

*/ 

CREATE TABLE main.dic_type_ev
(
  id_type_ev serial PRIMARY KEY,
  type_ev varchar(50) NOT NULL UNIQUE,
  desc_type_ev text
);

CREATE TABLE main.dic_activity(
  id_act serial PRIMARY KEY,
  activity varchar(100) NOT NULL UNIQUE,
  desc_activity text,
  cd_type_ev integer REFERENCES main.dic_type_ev(id_type_ev)
);

CREATE TABLE main.event
(
  id_ev serial PRIMARY KEY,
  date_ev date NOT NULL,
  time_ev timestamp,
  cd_act integer REFERENCES main.dic_activity(id_act) NOT NULL,
  cd_input integer REFERENCES main.event(id_ev),
  comments text,
  UNIQUE (date_ev,time_ev,cd_act,cd_input)
);

CREATE TABLE main.dic_type_gp
(
  id_type_gp serial PRIMARY KEY,
  type_gp text -- the idea here is to sepatate permanent plots, etc
);

CREATE TABLE main.dic_abund
(
  id_abund serial PRIMARY KEY,
  type_abund text
);

CREATE TABLE main.input
(
  cd_ev integer PRIMARY KEY REFERENCES main.event(id_ev),
  cd_intype integer REFERENCES main.dic_type_gp(id_type_gp) NOT NULL,
  cd_abund integer REFERENCES main.dic_abund(id_abund) NOT NULL,
  compo_table text UNIQUE,
  comment text 
);

/* TODO: For the persons it will be useful to manage different ways to integrate names, such as initials and all
I put an "initial" field for the cases where the first/middle names are not given, but we need to refine the CHECK options instead of a simple unique on all the table
*/
CREATE TABLE main.pers
(
  id_pers serial PRIMARY KEY,
  first_name varchar(50),
  middle_name varchar(50),
  last_name varchar(50) NOT NULL,
  last_name_2 varchar(50),
  initial varchar(6),
  email text UNIQUE,
  name_deter text,
  instit_current text,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE (first_name,middle_name,last_name,last_name_2,initial)
);

CREATE TABLE main.partic
(
  id_part serial PRIMARY KEY,
  cd_pers integer REFERENCES main.pers(id_pers) NOT NULL,
  cd_ev integer REFERENCES main.event(id_ev) NOT NULL,
  role_part text,
  order_role integer,
  institution text,
  sponsor text,
  comment text,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE (cd_pers,cd_ev,role_part)
);


/* The following table, even though it has been first thought as a group of table sharing taxonomic denomination might as well represent a group of plots which cannot be separated...
the consequences here are that the authorizations might as well not be separated between the plots, if it is accepted as that by the sib_users it might simplify greatly the process of rights
and permissions on the data

It will also define the type of abundance that might be stored for the group of plots. That also mean that there will be a correspondance in a group of plot: (<-- No it wont)
* for the authorization and habilitations of reading/writing etc
* for the denomination: if a denomination changes in a group, it changes for all the plots of the group
* for the type of abundance if this is a "composition-type" group of plots
*/

CREATE TABLE main.plot
(
  id_plot serial PRIMARY KEY,
  plot_name varchar(50) UNIQUE NOT NULL,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

CREATE TABLE main.plot_spatial
(
  gid integer PRIMARY KEY REFERENCES main.plot(id_plot)
);


CREATE TABLE main.plot_meta_verbatim
(
  id_plot int PRIMARY KEY REFERENCES main.plot(id_plot),
  latitude_decimal double precision,
  longitude_decimal double precision,
  altitude_m integer,
  area_ha double precision,
  area_type varchar(27),
  plot_type varchar(15),
  shape_type varchar(15),
  length_m integer,
  width_m integer,
  subplot_area_ha double precision,
  region varchar(50),
  ecosystem varchar(50), -- that should include the franja/habitat/gran ambiente/region_vida from O. Rangel's data (we might need to make it various fields)
  location_name text, -- let's check in the data whether this may correspond to the veredas
  location_type varchar(15),
  state varchar(15), -- difference between state, country, province
  country varchar(30),
  province varchar(30),
  terrain_type varchar(50),
  forest_composition varchar(50),
  substrate_geology varchar(50),
  forest_status varchar(50),
  forest_age_year varchar(6),
  general_slope_degree varchar(6),
  nearest_anthropogenic_edge integer,
  fragment_size_ha integer,
  comments text,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE(latitude_decimal,longitude_decimal)
);



CREATE TABLE main.sampling_unit
(
  id_su serial PRIMARY KEY,
  cd_plot int REFERENCES main.plot(id_plot) NOT NULL,
  name_su serial NOT NULL,
  type_hier_su varchar(50),
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE (cd_plot, name_su)
);

CREATE TABLE main.sampling_unit_spat
(
  gid_su integer REFERENCES main.sampling_unit(id_su)
);

CREATE TABLE main.fieldwork
(
  id_fw serial PRIMARY KEY,
  cd_su integer REFERENCES main.sampling_unit(id_su) NOT NULL,
  cd_ev integer REFERENCES main.event(id_ev),
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE(cd_su, cd_input)
);


CREATE TABLE main.dic_tax_level
(
  id_lev varchar(6) PRIMARY KEY,
  order_int integer NOT NULL UNIQUE,
  tax_level varchar(20) NOT NULL UNIQUE
);

CREATE TABLE main.taxonomy
(
  id_tax serial PRIMARY KEY,
  name varchar(100) NOT NULL UNIQUE,
  cd_lev varchar(6) REFERENCES main.dic_tax_level(id_lev),
  cd_sup integer REFERENCES main.taxonomy(id_tax),
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

/* TODO: In the denominations, we should have only one denomination by plot or tables, 
but right now the tables/plot are not referenced in the table...*/
/*TODO: Here we have a clear differerence between phytosociology and plots: 
in plots identification is done on one individual, in phytosociology on a group of individuals
However, both processes are very similar, so putting them both in the same table make sense
*/
CREATE TABLE main.tax_ident
(
  id_ident serial PRIMARY KEY,
  --cd_ind integer REFERENCES main.ind_census(id_ind),
  denom text NOT NULL,
  voucher varchar(11),
  catalog_number varchar(15),
  cd_tax integer REFERENCES main.taxonomy(id_tax),
  digital_voucher text,
  cd_record integer REFERENCES main.event(id_ev),
  cd_identification integer REFERENCES main.event(id_ev),
  comment text,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE(cd_input,denom)
);

CREATE TABLE main.ind_census
(
  id_ind serial PRIMARY KEY,
  ind_name varchar(5) NOT NULL,
  cd_fw integer REFERENCES main.fieldwork(id_fw) NOT NULL, -- maybe putting the fieldwork reference at the tag/ramet scale is sufficient and not needed here... need to think it over
  cd_ident integer REFERENCES main.tax_ident(id_ident) NOT NULL,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE(cd_input,cd_fw,ind_name)
);

CREATE TABLE main.xy_ind
(
  id_xy serial PRIMARY KEY,
  cd_ind integer REFERENCES main.ind_census(id_ind) NOT NULL UNIQUE,
  x_m double precision NOT NULL,
  y_m double precision NOT NULL,
  comments text,
  cd_fw integer REFERENCES main.fieldwork(id_fw) NOT NULL,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

CREATE TABLE main.tag_census
(
  id_tag serial PRIMARY KEY,
  cd_ind integer REFERENCES main.ind_census(id_ind) NOT NULL,
  cd_fw integer REFERENCES main.fieldwork(id_fw) NOT NULL, -- we need to keep that here and not only to reference the individual, because a ramet can appear in a particular fieldwork
  tag varchar(10) NOT NULL,
  ramet integer NOT NULL,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE(tag,cd_fw),-- ensure that tags are unique for a particular fieldwork event (in a cd_input to allow for keeping modifications)
  UNIQUE(cd_ind,ramet) -- ensure that ramet numbers are unique for an individual (in a cd_input to allow for keeping modifications)
);

CREATE TABLE main.tag_measurement
(
  id_measure serial PRIMARY KEY,
  cd_tag integer REFERENCES main.tag_census(id_tag) NOT NULL,
  cd_fw integer REFERENCES main.fieldwork(id_fw) NOT NULL,
  dbh_cm double precision,
  pom_cm double precision,
  height_cm double precision,
  alive_status varchar(10) NOT NULL, -- check whether categorizable
  comments text,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE(cd_tag,cd_fw)
);

/*
Should we merge these 2 tables (or should we consider alive all what is not dead better?)


CREATE TABLE main.tag_status
(
  id_status serial PRIMARY KEY,
  cd_tag integer REFERENCES main.tag_census(id_tag) NOT NULL,
  cd_fw integer REFERENCES main.fieldwork(id_fw) NOT NULL,
  alive_status varchar(10), --- check whether categorizable
  comments text,
  cd_input integer REFERENCES main.input(cd_ev),
  UNIQUE(cd_fw,cd_tag)
);

*/

CREATE TABLE main.tag_mortality
(
  id_mort serial PRIMARY KEY,
  cd_tag integer REFERENCES main.tag_census(id_tag) NOT NULL,
  --cd_status integer REFERENCES main.tag_status(id_status) NOT NULL UNIQUE,
  cd_fw integer REFERENCES main.fieldwork(id_fw) NOT NULL,
  mechanism varchar(30),
  mortality_type varchar(30),
  killer_process varchar(30),
  comment text,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL,
  UNIQUE(cd_tag)
);







CREATE TABLE main.compo
(
  id_com serial PRIMARY KEY,
  cd_fw int REFERENCES main.fieldwork(id_fw) NOT NULL,
  cd_ident int REFERENCES main.tax_ident(id_ident) NOT NULL,
  abund double precision,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

CREATE TABLE main.dic_syntax_level
(
  id_synlev varchar(6) PRIMARY KEY,
  order_int int NOT NULL UNIQUE,
  syntax_level varchar(20) NOT NULL UNIQUE,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

CREATE TABLE main.syntaxonomy
(
  id_syn serial PRIMARY KEY,
  name text NOT NULL,
  cd_synlev varchar(6) REFERENCES main.dic_syntax_level(id_synlev),
  cd_sup integer REFERENCES main.syntaxonomy(id_syn),
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

CREATE TABLE main.sp_carac_syn
(
  id_spcarac serial PRIMARY KEY,
  cd_ident int REFERENCES main.tax_ident(id_ident) NOT NULL,
  cd_syn int REFERENCES main.syntaxonomy(id_syn) NOT NULL,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

CREATE TABLE main.su_syn
(
  id_su_syn serial PRIMARY KEY,
  cd_su int REFERENCES main.sampling_unit(id_su) NOT NULL,
  cd_syn int REFERENCES main.syntaxonomy(id_syn) NOT NULL,
  cd_input integer REFERENCES main.input(cd_ev) NOT NULL
);

