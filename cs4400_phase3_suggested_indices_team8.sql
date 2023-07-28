-- CS4400 Phase 3 Extra Credit, Group 8 (Radeen Abree, Add your names here please if you are reading the file)

-- Suggested Index 1: 
	-- This index would improve the speed of the flight_takeoff, flight_landing, passengers_board stored procedures, and a number of others (10 total) in a number of ways
		-- For starters, the index would result in faster execution for a number of subqueries, as the subqueries checking for pilot shortages in both Jet airplanes and Propelleer-driven airplanes rely on the attributes being indexed.
		-- Along with this the flight table is joined with the airplane table in two instances in this stored procedure, therefore, by adding a composite index that includes the information needed for these joins the overall speed of joins will improve. 
		-- Finally, this index will speed up the constraint checks for the number of pilots needed to fly a plane, as the constraint checks for determining if there is a correct number of pilots present depends on the attribute being indexed, so this index would speed up constraints checks and thereby increase the speed of query execution time in the case that not enough pilots are present. 
    CREATE INDEX idx_flight_support_airline_tail ON flight (support_airline, support_tail);

	-- Despite the fact that this index would improve the performance of the database overall, it would slow down some views and stored procedures. Specifically, any views and stored procedures that do not involve support_airline and support_tail columns for filtering or joining would experience a slight drop in performance as a result of the overhead that the index adds to storage. 
		-- Specifically, the offer_flight stored procedure would slow down, as it writes to the flight table, which the index would slow down as indices slow down the writing process. 


-- Suggested Index 2: 
	-- This index would improve the speed of a number of tables, specifically this index would speed up operations involving any table that references the routeID as a foreign key. 
		-- In addition to speeding up the retrieval of the routeID foreign key, this index would result in minimal drawbacks for database performance, as the routeID of a given path would rarely change if at all. In addition to this, adding a new path is most likely not a common occurrence in the database, as adding items like passengers, tickets, and flights will occur significantly more often than adding routes, meaning a minimal drawback occurs as a result of this index. 
        -- Specifically, this index would benefit the flights_in_the_air stored procedure, as this procedure joins the flight table with the route_path and leg tables, for which indexing the RouteID would help quickly match corresponding tuples in these tables. 
        -- The simulation_cycle stored procedure would also benefit from this, as the stored procedure references the route_path table, which can be sped up. by indexing the routeID as well
        -- The route_summary view will also be sped up, as it involves joining tables based on the routeID, which will be sped up by indexing routeID 
    CREATE INDEX idx_route_path_routeID ON route_path (routeID);
    
    -- Although this index will improve overall database performance, it will slow down some aspects of the database, as the storage that it will take will marginally decrease the overall performance of the database. 
     -- Specifically, this index will slow down the start_route procedure, as this procedure writes to a table that contains routeID. As a result of this index writing into this table will have decreased performance. 


-- Suggested Index 3: 
	-- This index would speed up database performance as a whole, as there is high cardinality within the airport table with regard to the city attribute. Specifically, the airport table is joined with other tables with respect to the city attribute, therefore by indexing the city attribute of the airport table the speed of these joins will be increased. 
		-- Specifically, the alternative_airports view will benefit from this index, as it utilizes grouping operations based on the city attribute, meaning that indexing the city attribute will speed up the performance of this view. 
        -- The flights_on_the_ground view also involves grouping by the departing_from column, which relies on the city attribute, which will intern be sped up by indexing the city. 
        -- The people_on_the_ground view also relies on the airport column, which also relies on the city attribute, meaning that the performance of the view will be increased by the indexing of the city attribute. 
	CREATE INDEX idx_airport_city ON airport (city);
    
    -- Although this index is overall a net win for the database, it will decrease performance for procedures that write to the city attribute. 
     -- Specifically, this index will decrease the performance of the add_airport procedure, as this procedure writes to the city attribute. The write performance will experience an increased write time as a result of the index, but since new airports won't be added as often as the location of airports is referenced, this index is an overall positive for the database. 



