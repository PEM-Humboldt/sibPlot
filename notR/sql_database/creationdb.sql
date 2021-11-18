
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
  fecha date NOT NULL,
  time timestamp,
  cd_type_ev integer REFERENCES main.dic_type_ev(id_type_ev) NOT NULL,
  cd_act integer REFERENCES main.dic_activity(id_act),
  cd_input integer REFERENCES main.event(id_ev),
  comments text,
  UNIQUE (fecha,time,cd_type_ev)
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
  cd_entry integer REFERENCES main.event(id_ev),
  UNIQUE (first_name,middle_name,last_name,last_name_2,initial)
);

CREATE TABLE main.partic
(
  id serial PRIMARY KEY,
  cd_pers integer REFERENCES main.pers(id_pers),
  cd_ev integer REFERENCES main.event(id_ev),
  role text,
  order_role integer,
  institution text,
  sponsor text,
  comment text,
  email text,
  UNIQUE (cd_pers,cd_ev,role)
  --- note: not sure whether to put institutions, sponsors here or in a separate table
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

/* The following table, even though it has been first thought as a group of table sharing taxonomic denomination might as well represent a group of plots which cannot be separated...
the consequences here are that the authorizations might as well not be separated between the plots, if it is accepted as that by the sib_users it might simplify greatly the process of rights
and permissions on the data

It will also define the type of abundance that might be stored for the group of plots. That also mean that there will be a correspondance in a group of plot:
* for the authorization and habilitations of reading/writing etc
* for the denomination: if a denomination changes in a group, it changes for all the plots of the group
* for the type of abundance if this is a "composition-type" group of plots
*/
CREATE TABLE main.gp_plot -- note: in a group of plots the denomination of species is the same between the plots, it allows to take into account the fact that the species had only one name in a table of composition
(
  id_gp serial PRIMARY KEY,
  name_gp text,
  type_gp int REFERENCES main.dic_type_gp(id_type_gp),
  cd_abund int REFERENCES main.dic_abund(id_abund),
  comments text,
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.plot
(
  id_plot serial PRIMARY KEY,
  plot_name varchar(50) UNIQUE NOT NULL,
  cd_gp int REFERENCES main.gp_plot(id_gp),
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.plot_spat_pt
(
  id integer PRIMARY KEY REFERENCES main.plot(id_plot)
);


CREATE TABLE main.plot_spat_polyg
(
  id integer PRIMARY KEY REFERENCES main.plot(id_plot)
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
  ecosystem varchar(50),
  location_name text,
  location_tyoe varchar(15),
  state varchar(15),
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
  cd_input integer REFERENCES main.event(id_ev)
);



CREATE TABLE main.sampling_unit
(
  id_su serial PRIMARY KEY,
  cd_plot int REFERENCES main.plot(id_plot) NOT NULL,
  name_su serial NOT NULL,
  type_hier_su varchar(50),
  cd_input integer REFERENCES main.event(id_ev) NOT NULL
);

CREATE TABLE main.ind_census
(
  id_ind serial PRIMARY KEY,
  cd_su integer REFERENCES main.sampling_unit(id_su),
  code varchar(8),
  cd_input integer REFERENCES main.event(id_ev) NOT NULL,
  cd_event integer REFERENCES main.event(id_ev) NOT NULL
);

CREATE TABLE main.tag_census
(
  id_tag serial PRIMARY KEY,
  cd_ind integer REFERENCES main.ind_census(id_ind) NOT NULL,
  cd_su integer REFERENCES main.sampling_unit(id_su) NOT NULL,
  tag varchar(10),
  subplot integer,
  ramet integer,
  cd_input integer REFERENCES main.event(id_ev) NOT NULL,
  cd_event integer REFERENCES main.event(id_ev) NOT NULL,
  UNIQUE(tag,cd_su,cd_input)
);

CREATE TABLE main.tag_measurement
(
  id_measure serial PRIMARY KEY,
  cd_tag integer REFERENCES main.tag_census(id_tag) NOT NULL,
  dbh_cm double precision,
  pom_cm double precision,
  height_cm double precision,
  comments text,
  cd_input integer REFERENCES main.event(id_ev),
  cd_event integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.tag_status
(
  id_status serial PRIMARY KEY,
  cd_tag integer REFERENCES main.tag_census(id_tag),
  alive_status varchar(10), --- check whether categorizable
  comments text,
  cd_input integer REFERENCES main.event(id_ev),
  cd_event integer REFERENCES main.event(id_ev),
  UNIQUE(cd_event,cd_tag)
);

CREATE TABLE main.tag_mortality
(
  id_mort serial PRIMARY KEY,
  cd_status integer REFERENCES main.tag_status(id_status) NOT NULL UNIQUE,
  mechanism varchar(30),
  mortality_type varchar(30),
  killer_process varchar(30),
  comment text,
  cd_input integer REFERENCES main.event(id_ev),
  cd_event integer REFERENCES main.event(id_ev)
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
  name varchar(100) NOT NULL,
  cd_lev varchar(6) REFERENCES main.dic_tax_level(id_lev),
  cd_sup integer REFERENCES main.taxonomy(id_tax),
  cd_input integer REFERENCES main.event(id_ev)
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
  cd_ind integer REFERENCES main.ind_census(id_ind),
  denom text NOT NULL,
  cd_gp_plot int REFERENCES main.gp_plot(id_gp), --note: in cases where there are no "composition tables" including various "plots" it could reference directly a plot, not a group of plots
  voucher varchar(11),
  catalog_number varchar(15),
  cd_tax integer REFERENCES main.taxonomy(id_tax),
  digital_voucher text,
  cd_record integer REFERENCES main.event(id_ev),
  cd_identification integer REFERENCES main.event(id_ev),
  comment text,
  cd_input integer REFERENCES main.event(id_ev),
  UNIQUE(cd_input,cd_ind)
);

CREATE TABLE main.xy_tag
(
  id_xy serial PRIMARY KEY,
  cd_ind integer REFERENCES main.ind_census(id_ind),
  x_m double precision,
  y_m double precision,
  comments text,
  cd_input integer REFERENCES main.event(id_ev),
  UNIQUE(cd_input,cd_ind)
);

CREATE TABLE main.compo
(
  id_com serial PRIMARY KEY,
  cd_su int REFERENCES main.sampling_unit(id_su),
  cd_ident int REFERENCES main.tax_ident(id_ident),
  abund double precision,
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.dic_syntax_level
(
  id_synlev varchar(6) PRIMARY KEY,
  order_int int NOT NULL UNIQUE,
  syntax_level varchar(20) NOT NULL UNIQUE,
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.syntaxonomy
(
  id_syn serial PRIMARY KEY,
  name text NOT NULL,
  cd_synlev varchar(6) REFERENCES main.dic_syntax_level(id_synlev),
  cd_sup integer REFERENCES main.syntaxonomy(id_syn),
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.sp_carac_syn
(
  id_spcarac serial PRIMARY KEY,
  cd_gp int REFERENCES main.gp_plot(id_gp),
  cd_ident int REFERENCES main.tax_ident(id_ident),
  cd_syn int REFERENCES main.syntaxonomy(id_syn),
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.plot_syn
(
  id_plsyn serial PRIMARY KEY,
  cd_plot int REFERENCES main.plot(id_plot),
  cd_syn int REFERENCES main.syntaxonomy(id_syn),
  cd_input integer REFERENCES main.event(id_ev)
);
