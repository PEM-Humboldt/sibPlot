
INSERT INTO main.dic_type_ev(type_ev,desc_type_ev) VALUES
('data input','Insertion of data in the database'),
('field work','Collection of biological or environmental information in the field'),
('lab work','Analysis of data in a lab environment (species determination, environmental analysis, spatial analysis etc.)');

INSERT INTO main.dic_activity (activity,desc_activity,cd_type_ev) VALUES
('input_admin','Insertion of data from database managers',1),
('input_other','Insertion of data from users',1),
('census0','First census in the field',2),
('censusn','Other census in the field (note that census2,..n will be recorded as census1',2),
('trait0','First census in the field with trait measurement',2),
('traitn','Other census in the field with trait measurement (note that census2,..n will be recorded as census1',2),
('tax_identification','Taxonomic identification',3);

INSERT INTO main.dic_tax_level VALUES
  ('SSP',0,'Subspecies levels'),
  ('SP',4,'Species'),
  ('GN',5,'Genus'),
  ('FAM',7,'Family');
