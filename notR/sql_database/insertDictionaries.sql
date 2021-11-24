
INSERT INTO main.dic_type_ev(type_ev,desc_type_ev) VALUES
('data_input','Insertion of data in the database'),
('field_work','Collection of biological or environmental information in the field'),
('lab_work','Analysis of data in a lab environment (species determination, environmental analysis, spatial analysis etc.)');

INSERT INTO main.dic_activity (activity,desc_activity,cd_type_ev) VALUES
('input_admin','Insertion of data from database managers',1),
('input_spec',' Spec definition from database managers',1),
('input_other','Insertion of data from users',1),
('input_modif','Modification of existing data from users',1),
('census0','First census in the field',2),
('censusn','Other census in the field (note that census2,..n will be recorded as census1',2),
('trait0','First census in the field with trait measurement',2),
('traitn','Other census in the field with trait measurement (note that census2,..n will be recorded as census1',2),
('compo','Composition field work (no individual data but composition of local community)',2),
('tax_identification','Taxonomic identification',3),
('envir','Environmental data analysis',3),
('syn_ana','Syntaxonomic analysis',3);

INSERT INTO main.dic_tax_level VALUES
  ('SSP',0,'Subspecies levels'),
  ('SP',4,'Species'),
  ('GN',5,'Genus'),
  ('FAM',7,'Family');
