
/*Organization schema and authorizations etc.*/



/* Creation tables 
I will start replicating exactly Roy's structure, then we will think how to modify it

*/ 
CREATE TABLE main.plot_metadata
(
    id serial PRIMARY KEY,
    register_date date,
    latitude_decimal double,
    longitude_decimal double,
    altitude_m int,
    area_ha double,
    
