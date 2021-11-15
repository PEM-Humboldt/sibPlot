SELECT AddGeometryColumn('main','plot_spat_pt','the_geom',4326,'POINT',2); 
CREATE INDEX plot_pt_gix ON main.plot_spat_pt USING GIST(the_geom);

SELECT AddGeometryColumn('main','plot_spat_polyg','the_geom',4326,'POLYGON',2);
CREATE INDEX plot_poly_gix ON main.plot_spat_polyg USING GIST(the_geom);
