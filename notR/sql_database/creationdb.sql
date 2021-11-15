
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


/* TODO: For the persons it will be useful to manage different ways to integrate names, such as initials and all*/
CREATE TABLE main.pers
(
  id_pers serial PRIMARY KEY,
  first_name varchar(50) NOT NULL,
  middle_name varchar(50),
  last_name_1 varchar(50) NOT NULL,
  last_name_2 varchar(50),
  cd_entry integer REFERENCES main.event(id_ev),
  UNIQUE (first_name,middle_name,last_name)
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
  --- note: not sure whether to put institutions, sponsors here
);


CREATE TABLE main.plot
(
  id_plot serial PRIMARY KEY,
  plot_name varchar(50) UNIQUE NOT NULL,
  id_entry integer REFERENCES main.event(id_ev)
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

CREATE TABLE main.ind_census
(
  id_ind serial PRIMARY KEY,
  cd_plot integer REFERENCES main.plot(id_plot),
  code varchar(8),
  cd_input integer REFERENCES main.event(id_ev) NOT NULL,
  cd_event integer REFERENCES main.event(id_ev) NOT NULL
);

CREATE TABLE main.tag_census
(
  id_tag serial PRIMARY KEY,
  cd_ind integer REFERENCES main.ind_census(id_ind) NOT NULL,
  cd_plot integer REFERENCES main.plot(id_plot) NOT NULL,
  tag varchar(10),
  subplot integer,
  ramet integer,
  cd_input integer REFERENCES main.event(id_ev) NOT NULL,
  cd_event integer REFERENCES main.event(id_ev) NOT NULL,
  UNIQUE(tag,cd_plot)
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
  order_int integer NOT NULL,
  tax_level varchar(20) NOT NULL
);

CREATE TABLE main.taxonomy
(
  id_tax serial PRIMARY KEY,
  name varchar(100) NOT NULL,
  cd_lev varchar(6) REFERENCES main.dic_tax_level(id_lev),
  cd_sup integer REFERENCES main.taxonomy(id_tax),
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.tax_ident
(
  id_ident serial PRIMARY KEY,
  cd_ind integer REFERENCES main.ind_census(id_ind),
  voucher varchar(11),
  catalog_number varchar(15),
  cd_tax integer REFERENCES main.taxonomy(id_tax),
  digital_voucher text,
  cd_record integer REFERENCES main.event(id_ev),
  cd_ident integer REFERENCES main.event(id_ev),
  comment text,
  cd_input integer REFERENCES main.event(id_ev)
);

CREATE TABLE main.xy_tag
(
  id_xy serial PRIMARY KEY,
  cd_tag integer REFERENCES main.tag_census(id_tag),
  x_m double precision,
  y_m double precision,
  comments text,
  cd_input integer REFERENCES main.event(id_ev)
);
