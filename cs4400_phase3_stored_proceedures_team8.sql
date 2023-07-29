-- CS4400: Introduction to Database Systems: Wednesday, March 8, 2023
-- Flight Management Course Project Mechanics (v1.0) STARTING SHELL
-- Views, Functions & Stored Procedures

/* This is a standard preamble for most of our scripts.  The intent is to establish
a consistent environment for the database behavior. */
set global transaction isolation level serializable;
set global SQL_MODE = 'ANSI,TRADITIONAL';
set names utf8mb4;
set SQL_SAFE_UPDATES = 0;
set @thisDatabase = 'flight_management';

use flight_management;
-- -----------------------------------------------------------------------------
-- stored procedures and views
-- -----------------------------------------------------------------------------
/* Standard Procedure: If one or more of the necessary conditions for a procedure to
be executed is false, then simply have the procedure halt execution without changing
the database state. Do NOT display any error messages, etc. */

-- [1] add_airplane()
-- -----------------------------------------------------------------------------
/* This stored procedure creates a new airplane.  A new airplane must be sponsored
by an existing airline, and must have a unique tail number for that airline.
username.  An airplane must also have a non-zero seat capacity and speed. An airplane
might also have other factors depending on it's type, like skids or some number
of engines.  Finally, an airplane must have a database-wide unique location if
it will be used to carry passengers. */
-- -----------------------------------------------------------------------------
drop procedure if exists add_airplane;
delimiter //
create procedure add_airplane (in ip_airlineID varchar(50), in ip_tail_num varchar(50),
	in ip_seat_capacity integer, in ip_speed integer, in ip_locationID varchar(50),
    in ip_plane_type varchar(100), in ip_skids boolean, in ip_propellers integer,
    in ip_jet_engines integer)
sp_main: begin

DECLARE airlineExists INT;
DECLARE tailNumExists INT;
DECLARE validLocation INT;

-- Check Airline Sponsorship
SELECT COUNT(*) INTO airlineExists FROM airline WHERE airlineID = ip_airlineID;

-- Check Unique Tail Number
SELECT COUNT(*) INTO tailNumExists FROM airplane WHERE airlineID = ip_airlineID AND tail_num = ip_tail_num;

-- Check Seat Capacity and Speed
IF ip_seat_capacity = 0 OR ip_speed = 0 THEN
    SET airlineExists = 0;
END IF;

-- Check Airplane Location Identifier 
IF ip_locationID IS NOT NULL THEN
    SELECT COUNT(DISTINCT locationID) INTO validLocation FROM airplane WHERE locationID = ip_locationID;
    IF validLocation > 0 THEN
        SET airlineExists = 0;
    END IF;
    
    IF NOT EXISTS (SELECT * FROM location WHERE locationID = ip_locationID AND locationID LIKE 'plane%') THEN
        SET airlineExists = 0;
    END IF;
END IF;

IF airlineExists = 0 THEN
    LEAVE sp_main;
END IF;

-- Airplane Insert Statement   
INSERT INTO airplane (airlineID, tail_num, seat_capacity, speed, locationID, plane_type, skids, propellers, jet_engines) VALUES (ip_airlineID, ip_tail_num, ip_seat_capacity, ip_speed, ip_locationID, ip_plane_type, ip_skids, ip_propellers, ip_jet_engines);

-- Location Insert Statement 
IF ip_locationID IS NOT NULL AND NOT EXISTS (SELECT * FROM location WHERE locationID = ip_locationID AND locationID LIKE 'plane%') THEN
    INSERT INTO location (locationID) VALUES (ip_locationID);
END IF;

end //
delimiter ;

-- [2] add_airport()
-- -----------------------------------------------------------------------------
/* This stored procedure creates a new airport.  A new airport must have a unique
identifier along with a database-wide unique location if it will be used to support
airplane takeoffs and landings.  An airport may have a longer, more descriptive
name.  An airport must also have a city and state designation. */
-- -----------------------------------------------------------------------------
drop procedure if exists add_airport;
delimiter //
create procedure add_airport (in ip_airportID char(3), in ip_airport_name varchar(200),
    in ip_city varchar(100), in ip_state char(2), in ip_locationID varchar(50))
sp_main: begin

-- Airport Unique ID and Location ID
IF ip_airportID IS NOT NULL AND EXISTS (SELECT * FROM airport WHERE airportID = ip_airportID) THEN
    LEAVE sp_main;
END IF;

IF ip_locationID IS NOT NULL AND (EXISTS (SELECT * FROM airport WHERE locationID = ip_locationID) OR EXISTS (SELECT * FROM location WHERE locationID = ip_locationID AND locationID LIKE 'port%')) THEN
    LEAVE sp_main;
END IF;

-- City and State
IF ip_city IS NULL OR ip_state IS NULL THEN
    LEAVE sp_main;
END IF;

-- Airport Insert Statement
INSERT INTO airport (airportID, airport_name, city, state, locationID) VALUES (ip_airportID, ip_airport_name, ip_city, ip_state, ip_locationID);

-- Location Insert Statement
IF ip_locationID IS NOT NULL AND NOT EXISTS (SELECT * FROM location WHERE locationID = ip_locationID AND locationID LIKE 'port%') THEN
    INSERT INTO location (locationID) VALUES (ip_locationID);
END IF;

end // 
delimiter ;


-- [3] add_person()
-- -----------------------------------------------------------------------------
/* This stored procedure creates a new person.  A new person must reference a unique
identifier along with a database-wide unique location used to determine where the
person is currently located: either at an airport, or on an airplane, at any given
time.  A person may have a first and last name as well.

Also, a person can hold a pilot role, a passenger role, or both roles.  As a pilot,
a person must have a tax identifier to receive pay, and an experience level.  Also,
a pilot might be assigned to a specific airplane as part of the flight crew.  As a
passenger, a person will have some amount of frequent flyer miles. */
-- -----------------------------------------------------------------------------
drop procedure if exists add_person;
delimiter //

create procedure add_person (in ip_personID varchar(50), in ip_first_name varchar(100),
    in ip_last_name varchar(100), in ip_locationID varchar(50), in ip_taxID varchar(50),
    in ip_experience integer, in ip_flying_airline varchar(50), in ip_flying_tail varchar(50),
    in ip_miles integer)
sp_main: begin

DECLARE is_person_exists INT;
DECLARE is_location_valid INT;

-- Unique PersonID
SELECT COUNT(*) INTO is_person_exists FROM person WHERE personID = ip_personID;
IF ip_personID IS NULL OR is_person_exists > 0 THEN
    LEAVE sp_main;
END IF;

-- Location Constraints
SELECT COUNT(*) INTO is_location_valid FROM location WHERE locationID = ip_locationID;
IF ip_locationID IS NULL OR is_location_valid = 0 THEN
    LEAVE sp_main;
END IF;

-- Person Insert Statement
INSERT INTO person (personID, first_name, last_name, locationID)
VALUES (ip_personID, ip_first_name, ip_last_name, ip_locationID);

-- Identify passengers and mile constraints
IF ip_miles IS NOT NULL THEN
    INSERT INTO passenger (personID, miles) VALUES (ip_personID, ip_miles);
END IF;

-- Identify pilots and taxID constraints as a pilot
IF ip_taxID IS NOT NULL THEN
    INSERT INTO pilot (personID, taxID, experience, flying_airline, flying_tail)
    VALUES (ip_personID, ip_taxID, ip_experience, ip_flying_airline, ip_flying_tail);
END IF;

end //
delimiter ;


-- [4] grant_pilot_license()
-- -----------------------------------------------------------------------------
/* This stored procedure creates a new pilot license.  The license must reference
a valid pilot, and must be a new/unique type of license for that pilot. */
-- -----------------------------------------------------------------------------
drop procedure if exists grant_pilot_license;
delimiter //
create procedure grant_pilot_license (in ip_personID varchar(50), in ip_license varchar(100))
sp_main: begin

-- Valid pilot constraint
IF NOT EXISTS (SELECT * FROM pilot WHERE personID = ip_personID) THEN
    LEAVE sp_main;
END IF;

-- License is unique
IF EXISTS (SELECT * FROM pilot_licenses WHERE personID = ip_personID AND license = ip_license) THEN
    LEAVE sp_main;
END IF;

-- License insert statement
INSERT INTO pilot_licenses (personID, license) VALUES (ip_personID, ip_license);

end //
delimiter ;


-- [5] offer_flight()
-- -----------------------------------------------------------------------------
/* This stored procedure creates a new flight.  The flight can be defined before
an airplane has been assigned for support, but it must have a valid route.  Once
an airplane has been assigned, we must also track where the airplane is along
the route, whether it is in flight or on the ground, and when the next action -
takeoff or landing - will occur. */
-- -----------------------------------------------------------------------------
drop procedure if exists offer_flight;
delimiter //
create procedure offer_flight (in ip_flightID varchar(50), in ip_routeID varchar(50),
    in ip_support_airline varchar(50), in ip_support_tail varchar(50), in ip_progress integer,
    in ip_airplane_status varchar(100), in ip_next_time time)
sp_main: begin

-- Flight-route constraint 
IF NOT EXISTS (SELECT * FROM route WHERE routeID = ip_routeID) THEN
    LEAVE sp_main;
END IF;

INSERT INTO flight (flightID, routeID, support_airline, support_tail, progress, airplane_status, next_time) VALUES (ip_flightID, ip_routeID, ip_support_airline, ip_support_tail, ip_progress, ip_airplane_status, ip_next_time);

end //
delimiter ;


-- [6] purchase_ticket_and_seat()
-- -----------------------------------------------------------------------------
/* This stored procedure creates a new ticket.  The cost of the flight is optional
since it might have been a gift, purchased with frequent flyer miles, etc.  Each
flight must be tied to a valid person for a valid flight.  Also, we will make the
(hopefully simplifying) assumption that the departure airport for the ticket will
be the airport at which the traveler is currently located.  The ticket must also
explicitly list the destination airport, which can be an airport before the final
airport on the route.  Finally, the seat must be unoccupied. */
-- -----------------------------------------------------------------------------
drop procedure if exists purchase_ticket_and_seat;
delimiter //
create procedure purchase_ticket_and_seat (in ip_ticketID varchar(50), in ip_cost integer,
	in ip_carrier varchar(50), in ip_customer varchar(50), in ip_deplane_at char(3),
    in ip_seat_number varchar(50))
sp_main: begin

-- Check if the flight is tied to a valid person, the plane must deplane from somewhere, destination airport is listed and same as arrival airport, flight is in the leg of a route
IF (ip_customer IN 
	(SELECT personID 
    FROM person) AND ip_carrier IN 
		(SELECT flightID 
        FROM flight) AND ip_deplane_at IS NOT NULL AND NOT EXISTS (
			SELECT * 
            FROM flight AS f
			JOIN route_path AS rp ON f.routeID = rp.routeID
			JOIN leg AS l ON rp.legID = l.legID
			WHERE f.flightID = ip_deplane_at AND l.arrival = ip_deplane_at)
) THEN
    INSERT IGNORE INTO ticket (ticketID, cost, carrier, customer, deplane_at) VALUES (ip_ticketID, ip_cost, ip_carrier, ip_customer, ip_deplane_at);
END IF;

-- Edge Case: Seat number must be in the list of available tickets
IF NOT EXISTS (SELECT * FROM ticket_seats WHERE seat_number = ip_seat_number) THEN
    INSERT IGNORE INTO ticket_seats (ticketID, seat_number) VALUES (ip_ticketID, ip_seat_number);
END IF;

end //
delimiter ;


-- [7] add_update_leg()
-- -----------------------------------------------------------------------------
/* This stored procedure creates a new leg as specified.  However, if a leg from
the departure airport to the arrival airport already exists, then don't create a
new leg - instead, update the existence of the current leg while keeping the existing
identifier.  Also, all legs must be symmetric.  If a leg in the opposite direction
exists, then update the distance to ensure that it is equivalent.   */
-- -----------------------------------------------------------------------------
drop procedure if exists add_update_leg;
delimiter //
create procedure add_update_leg (in ip_legID varchar(50), in ip_distance integer,
    in ip_departure char(3), in ip_arrival char(3))
sp_main: begin

-- Check if the (departure, arrival) combination exists in the leg table
    IF (ip_departure, ip_arrival) IN (SELECT departure, arrival FROM leg) THEN
        -- Update the leg distance for the existing (departure, arrival) combination
        UPDATE leg
        SET distance = ip_distance
        WHERE departure = ip_departure AND arrival = ip_arrival;
    ELSE
        -- Insert a new row into the leg table
        INSERT INTO leg (legID, distance, departure, arrival)
        VALUES (ip_legID, ip_distance, ip_departure, ip_arrival)
        ON DUPLICATE KEY UPDATE distance = ip_distance; -- This line handles the update on duplicate key
    END IF;

    -- Check if the (arrival, departure) combination exists in the leg table
    IF (ip_arrival, ip_departure) IN (SELECT departure, arrival FROM leg) THEN
        -- Update the leg distance for the existing (arrival, departure) combination
        UPDATE leg
        SET distance = ip_distance
        WHERE departure = ip_arrival AND arrival = ip_departure;
	
    END IF;

end //
delimiter ;

-- [8] start_route()
-- -----------------------------------------------------------------------------
/* This stored procedure creates the first leg of a new route.  Routes in our
system must be created in the sequential order of the legs.  The first leg of
the route can be any valid leg. */
-- -----------------------------------------------------------------------------
drop procedure if exists start_route;
delimiter //
create procedure start_route (in ip_routeID varchar(50), in ip_legID varchar(50))
sp_main: begin

-- Route and Path insert statement
INSERT INTO route (routeID) VALUES (ip_routeID);
INSERT INTO route_path (routeID, legID, sequence) VALUES (ip_routeID, ip_legID, 1);
    
end //
delimiter ;


-- [9] extend_route()
-- -----------------------------------------------------------------------------
/* This stored procedure adds another leg to the end of an existing route.  Routes
in our system must be created in the sequential order of the legs, and the route
must be contiguous: the departure airport of this leg must be the same as the
arrival airport of the previous leg. */
-- -----------------------------------------------------------------------------
drop procedure if exists extend_route;
delimiter //
create procedure extend_route (in ip_routeID varchar(50), in ip_legID varchar(50))
sp_main: begin

-- Check if ip_legID and ip_routeID are not NULL
IF ip_legID IS NOT NULL AND ip_routeID IS NOT NULL THEN
    BEGIN
        -- Finding max of sequence
        SET @max_sequence = (
            SELECT MAX(sequence)
            FROM route_path
            WHERE routeID = ip_routeID
            GROUP BY routeID
        );

        -- Departure airport
        SET @new_leg_departure = (
            SELECT departure
            FROM leg
            WHERE legID = ip_legID
        );

        -- Identifying leg
        SET @leg = (
            SELECT legID
            FROM route_path
            WHERE routeID = ip_routeID AND sequence = @max_sequence
        );

        -- Check if the leg exists
        IF @leg IS NOT NULL THEN
            BEGIN
                -- Arrival airport must be departure airport
                SET @route_arrival = (
                    SELECT arrival
                    FROM leg
                    WHERE legID = @leg
                );

                -- Check if the arrival airport matches the departure airport of the new leg
                IF @route_arrival = @new_leg_departure THEN
                    BEGIN
                        -- Inserting leg into route if not already in it
                        IF ip_legID NOT IN (
                            SELECT legID
                            FROM route_path
                            WHERE routeID = ip_routeID
                        ) THEN
                            BEGIN
                                INSERT INTO route_path (routeID, legID, sequence)
                                VALUES (ip_routeID, ip_legID, @max_sequence + 1);
                            END;
                        END IF;
                    END;
                END IF;
            END;
        END IF;
    END;
END IF;



end //

delimiter ;




-- [10] flight_landing()
-- -----------------------------------------------------------------------------
/* This stored procedure updates the state for a flight landing at the next airport
along it's route.  The time for the flight should be moved one hour into the future
to allow for the flight to be checked, refueled, restocked, etc. for the next leg
of travel.  Also, the pilots of the flight should receive increased experience, and
the passengers should have their frequent flyer miles updated. */
-- -----------------------------------------------------------------------------
drop procedure if exists flight_landing;
delimiter //
create procedure flight_landing (in ip_flightID varchar(50))
sp_main: begin

-- Check if the flightID exists in the flight table
IF NOT EXISTS (SELECT flightID FROM flight WHERE flightID = ip_flightID) THEN
    LEAVE sp_main;
END IF;

-- Calculate the new time
UPDATE flight
SET
    airplane_status = 'on_ground',
    next_time = ADDTIME(next_time, '01:00:00')
WHERE
    flightID = ip_flightID;

-- Increase pilot experience
UPDATE pilot AS p
SET experience = experience + 1
WHERE EXISTS (
    SELECT 1
    FROM flight AS f
    WHERE f.support_airline = p.flying_airline AND f.support_tail = p.flying_tail
    AND f.flightID = ip_flightID
);

-- Update passenger miles
UPDATE passenger AS ps
JOIN ticket AS t ON ps.personID = t.customer
SET ps.miles = ps.miles + (
    SELECT distance
    FROM leg AS l
    JOIN route_path AS r ON l.legID = r.legID
    WHERE r.routeID = (SELECT routeID FROM flight WHERE flightID = ip_flightID)
    AND r.sequence = (SELECT progress FROM flight WHERE flightID = ip_flightID)
)
WHERE t.carrier = ip_flightID;

end //
delimiter ;



-- [11] flight_takeoff()
-- -----------------------------------------------------------------------------
/* This stored procedure updates the state for a flight taking off from its current
airport towards the next airport along it's route.  The time for the next leg of
the flight must be calculated based on the distance and the speed of the airplane.
And we must also ensure that propeller driven planes have at least one pilot
assigned, while jets must have a minimum of two pilots. If the flight cannot take
off because of a pilot shortage, then the flight must be delayed for 30 minutes. */
-- -----------------------------------------------------------------------------
drop procedure if exists flight_takeoff;
delimiter //
create procedure flight_takeoff (in ip_flightID varchar(50))
sp_main: begin

-- Constraint: Flight must exist 
DECLARE nextDuration INT DEFAULT 0;

IF NOT EXISTS (SELECT flightID FROM flight WHERE flightID = ip_flightID) THEN
    LEAVE sp_main;
END IF;

-- Checking for pilot shortage - jet 
IF (
    SELECT plane_type 
    FROM flight AS f 
    JOIN airplane AS a ON f.support_airline = a.airlineID AND f.support_tail = a.tail_num 
    WHERE f.flightID = ip_flightID
) = 'jet' AND (
    SELECT COUNT(p.personID) 
    FROM pilot AS p
    WHERE p.flying_airline = (
        SELECT support_airline 
        FROM flight 
        WHERE flightID = ip_flightID
    ) AND p.flying_tail = (
        SELECT support_tail 
        FROM flight 
        WHERE flightID = ip_flightID
    )
) < 2 THEN
    -- If shortage then delay flight
    UPDATE flight SET next_time = next_time + INTERVAL 30 MINUTE WHERE flightID = ip_flightID;
    LEAVE sp_main;
END IF;

-- Checking for pilot shortage - propeller 
IF (
    SELECT plane_type 
    FROM flight AS f 
    JOIN airplane AS a ON f.support_airline = a.airlineID AND f.support_tail = a.tail_num 
    WHERE f.flightID = ip_flightID
) = 'prop' AND (
    SELECT COUNT(p.personID) 
    FROM pilot AS p
    WHERE p.flying_airline = (
        SELECT support_airline 
        FROM flight 
        WHERE flightID = ip_flightID
    ) AND p.flying_tail = (
        SELECT support_tail 
        FROM flight 
        WHERE flightID = ip_flightID
    )
) < 1 THEN
    -- If shortage delay flight
    UPDATE flight SET next_time = next_time + INTERVAL 30 MINUTE WHERE flightID = ip_flightID;
    LEAVE sp_main;
END IF;

-- Shortages accounted for, update flight progress and airplane status 
UPDATE flight 
SET 
    progress = progress + 1,
    airplane_status = 'in_flight'
WHERE flightID = ip_flightID;

-- Find flight duration 
SET nextDuration = (
    SELECT l.distance / a.speed 
    FROM flight AS f 
    JOIN route_path AS r ON f.routeID = r.routeID AND f.progress = r.sequence 
    JOIN leg AS l ON r.legID = l.legID 
    JOIN airplane AS a ON f.support_airline = a.airlineID AND f.support_tail = a.tail_num 
    WHERE f.flightID = ip_flightID
);

-- Next flight updated
UPDATE flight SET next_time = next_time + INTERVAL nextDuration HOUR WHERE flightID = ip_flightID;

end //
delimiter ;


-- [12] passengers_board()
-- -----------------------------------------------------------------------------
/* This stored procedure updates the state for passengers getting on a flight at
its current airport.  The passengers must be at the airport and hold a valid ticket
for the flight. */
-- -----------------------------------------------------------------------------
drop procedure if exists passengers_board;
delimiter //
create procedure passengers_board (in ip_flightID varchar(50))
sp_main: begin

-- Update person location if their location matches the flight location
UPDATE person AS p
JOIN ticket AS t ON p.personID = t.customer
JOIN flight AS f ON t.carrier = f.flightID
JOIN airplane AS a ON f.support_airline = a.airlineID AND f.support_tail = a.tail_num
JOIN route_path AS r ON f.routeID = r.routeID
JOIN leg AS l ON r.legID = l.legID
SET p.locationID = a.locationID
WHERE f.flightID = ip_flightID
AND p.locationID = CASE
    WHEN f.progress != 0 THEN (SELECT arrival FROM flight AS f2 JOIN route_path AS r2 ON f2.routeID = r2.routeID AND f2.progress = r2.sequence JOIN leg AS l2 ON r2.legID = l2.legID WHERE f2.flightID = ip_flightID)
    ELSE (SELECT departure FROM flight AS f3 JOIN route_path AS r3 ON f3.routeID = r3.routeID AND f3.flightID = ip_flightID JOIN leg AS l3 ON r3.legID = l3.legID WHERE r3.sequence = 1)
    END;

end //
delimiter ;

-- [13] passengers_disembark()
-- -----------------------------------------------------------------------------
/* This stored procedure updates the state for passengers getting off of a flight
at its current airport.  The passengers must be on that flight, and the flight must
be located at the destination airport as referenced by the ticket. */
-- -----------------------------------------------------------------------------
drop procedure if exists passengers_disembark;
delimiter //
create procedure passengers_disembark (in ip_flightID varchar(50))
sp_main: begin

-- Create temporary table to find people whose flights arrived at their ticketed destinations and update their location
DROP TEMPORARY TABLE IF EXISTS table_temp;
CREATE TEMPORARY TABLE table_temp AS 
SELECT 
    flightID,
    personID,
    f.locationID,
    is_at,
    airport.locationID AS new_loc
FROM
    (SELECT 
        to_disembark_loc.flightID,
        personID,
        to_disembark_loc.locationID,
        is_at
    FROM
        (SELECT 
            flightID,
            personID,
            locationID,
            is_at
        FROM
            (SELECT 
                flightID,
                customer,
                is_at
            FROM
                (SELECT 
                    flightID,
                    arrival AS is_at
                FROM 
                    (SELECT 
                        flightID,
                        progress,
                        airplane_status,
                        legID
                    FROM 
                        flight
                    JOIN route_path ON flight.routeID = route_path.routeID
                    WHERE 
                        progress = sequence
                        AND airplane_status = 'on_ground'
                    ) flits_on_ground
                JOIN leg ON flits_on_ground.legID = leg.legID
                ) flit_loc
            JOIN ticket ON flit_loc.flightID = ticket.carrier
            WHERE 
                is_at = deplane_at
            ) pass_to_leave
        JOIN person ON pass_to_leave.customer = person.personID
        ) to_disembark_loc
    JOIN 
        (SELECT 
            flightID,
            locationID
        FROM 
            flight
        JOIN airplane ON flight.support_tail = airplane.tail_num
        ) flit_loc
    ON to_disembark_loc.flightID = flit_loc.flightID
    WHERE 
        to_disembark_loc.locationID = flit_loc.locationID
    ) f
JOIN airport ON f.is_at = airport.airportID
WHERE 
    flightID = ip_flightID;

SET @location = (SELECT new_loc FROM table_temp WHERE flightID = ip_flightID LIMIT 1);

UPDATE person 
SET locationID = @location
WHERE personID IN (SELECT personID FROM table_temp);

end //
delimiter ;

-- [14] assign_pilot()
-- -----------------------------------------------------------------------------
/* This stored procedure assigns a pilot as part of the flight crew for a given
airplane.  The pilot being assigned must have a license for that type of airplane,
and must be at the same location as the flight.  Also, a pilot can only support
one flight (i.e. one airplane) at a time.  The pilot must be assigned to the flight
and have their location updated for the appropriate airplane. */
-- -----------------------------------------------------------------------------
drop procedure if exists assign_pilot;
delimiter //
create procedure assign_pilot (in ip_flightID varchar(50), ip_personID varchar(50))
sp_main: begin

-- Assuring the assigned pilot has the required license for the plane type
IF NOT EXISTS (
    SELECT 1
    FROM flight AS f
    JOIN airplane AS a ON f.support_airline = a.airlineID AND f.support_tail = a.tail_num
    JOIN pilot AS p ON p.personID = ip_personID
    JOIN pilot_licenses AS pl ON p.personID = pl.personID
    WHERE f.flightID = ip_flightID AND a.plane_type = pl.license
) THEN
    LEAVE sp_main;
END IF;

-- Assuring pilot and plane location match
IF NOT EXISTS (
    SELECT 1
    FROM flight AS f
    JOIN route_path AS r ON r.routeID = f.routeID
    JOIN leg AS l ON r.legID = l.legID
    JOIN airport AS a ON l.arrival = a.airportID
    JOIN pilot AS p ON p.personID = ip_personID
    JOIN person AS per ON p.personID = per.personID
    WHERE f.flightID = ip_flightID AND f.progress = r.sequence AND per.locationID = a.locationID
) THEN
    LEAVE sp_main;
END IF;

-- Assuring pilot is only on one flight
IF EXISTS (
    SELECT 1
    FROM pilot
    WHERE personID = ip_personID AND (flying_airline IS NOT NULL OR flying_tail IS NOT NULL)
) THEN
    LEAVE sp_main;
END IF;

-- Updating pilot tables
UPDATE pilot
SET
    flying_airline = (SELECT support_airline FROM flight WHERE flightID = ip_flightID),
    flying_tail = (SELECT support_tail FROM flight WHERE flightID = ip_flightID)
WHERE personID = ip_personID;

UPDATE person
SET
    locationID = (SELECT a.locationID FROM flight AS f JOIN airplane AS a ON f.support_airline = a.airlineID AND f.support_tail = a.tail_num WHERE flightID = ip_flightID)
WHERE personID = ip_personID;

end //
delimiter ;

-- [15] recycle_crew()
-- -----------------------------------------------------------------------------
/* This stored procedure releases the assignments for a given flight crew.  The
flight must have ended, and all passengers must have disembarked. */
-- -----------------------------------------------------------------------------
drop procedure if exists recycle_crew;
delimiter //
create procedure recycle_crew (in ip_flightID varchar(50))
sp_main: begin

	-- Variable declarations
    DECLARE endLeg INT DEFAULT 0;
    DECLARE arrivalLocation VARCHAR(10);
    DECLARE totalPeople INT DEFAULT 0;
    DECLARE disembarkedP INT DEFAULT 0;

    -- Get the maximum sequence number for the route
    SELECT MAX(sequence) INTO endLeg
    FROM flight AS f
    JOIN route_path AS r ON f.routeID = r.routeID
    WHERE f.flightID = ip_flightID;

    -- Check if the flight's progress matches the end leg sequence
    IF endLeg != (SELECT progress FROM flight WHERE flightID = ip_flightID) THEN
        LEAVE sp_main;
    END IF;

    -- Check if the flight is on the ground
    IF ip_flightID NOT IN (SELECT flightID FROM flight WHERE airplane_status = 'on_ground') THEN
        LEAVE sp_main;
    END IF;

    -- Get the arrival location of the flight
    SELECT a.locationID INTO arrivalLocation
    FROM flight AS f
    JOIN route_path AS r ON f.routeID = r.routeID
    JOIN leg AS l ON r.legID = l.legID
    JOIN airport AS a ON l.arrival = a.airportID
    WHERE f.flightID = ip_flightID AND f.progress = r.sequence;

    -- Get the total passenger count and disembarked passenger count
    SELECT COUNT(t.customer) INTO totalPeople
    FROM flight AS f
    JOIN ticket AS t ON f.flightID = t.carrier
    WHERE f.flightID = ip_flightID;

    SELECT COUNT(t.customer) INTO disembarkedP
    FROM flight AS f
    JOIN ticket AS t ON f.flightID = t.carrier
    JOIN person AS p ON t.customer = p.personID
    WHERE f.flightID = ip_flightID AND p.locationID = arrivalLocation;

    -- Check if all passengers have disembarked
    IF totalPeople != disembarkedP THEN
        LEAVE sp_main;
    END IF;

    -- Update pilot location
    UPDATE person AS p
    JOIN pilot AS pt ON p.personID = pt.personID
    JOIN flight AS f ON f.support_airline = pt.flying_airline AND f.support_tail = pt.flying_tail
    SET p.locationID = arrivalLocation
    WHERE f.flightID = ip_flightID;

    -- Update pilot's flying_airline and flying_tail to NULL
    UPDATE pilot AS pt
    JOIN flight AS f ON f.support_airline = pt.flying_airline AND f.support_tail = pt.flying_tail
    SET pt.flying_airline = NULL, pt.flying_tail = NULL
    WHERE f.flightID = ip_flightID;
    
end //
delimiter ;

-- [16] retire_flight()
-- -----------------------------------------------------------------------------
/* This stored procedure removes a flight that has ended from the system.  The
flight must be on the ground, and either be at the start its route, or at the
end of its route.  */
-- -----------------------------------------------------------------------------
drop procedure if exists retire_flight;
delimiter //
create procedure retire_flight (in ip_flightID varchar(50))
sp_main: begin

-- Check if the flight is grounded or at destination
IF NOT EXISTS (
    SELECT 1
    FROM flight AS f
    JOIN route_path AS r ON f.routeID = r.routeID
    WHERE f.flightID = ip_flightID AND airplane_status = 'on_ground' AND (progress = 0 OR progress = (SELECT max(sequence) FROM route_path WHERE routeID = f.routeID))
) THEN
    LEAVE sp_main;
END IF;

-- Retire flight 
DELETE FROM flight WHERE flightID = ip_flightID;

end //
delimiter ;

-- [17] remove_passenger_role()
-- -----------------------------------------------------------------------------
/* This stored procedure removes the passenger role from person.  The passenger
must be on the ground at the time; and, if they are on a flight, then they must
disembark the flight at the current airport.  If the person had both a pilot role
and a passenger role, then the person and pilot role data should not be affected.
If the person only had a passenger role, then all associated person data must be
removed as well. */
-- -----------------------------------------------------------------------------
drop procedure if exists remove_passenger_role;
delimiter //
create procedure remove_passenger_role (in ip_personID varchar(50))
sp_main: begin

DECLARE onGround INT;
DECLARE isPass INT;

-- Edge case: Passenger must exist 
SELECT 1 INTO isPass FROM passenger WHERE personID = ip_personID;

IF isPass IS NULL THEN
    LEAVE sp_main;
END IF;

-- Passenger is on the ground 
SELECT 1 INTO onGround
FROM passenger
WHERE personID = ip_personID AND personID IN (SELECT personID FROM person WHERE locationID LIKE '%plane%');

IF onGround IS NOT NULL THEN
    LEAVE sp_main;
END IF;

-- Remove passenger information (except for pilot)
IF ip_personID NOT IN (SELECT personID FROM pilot) THEN
    DELETE FROM passenger WHERE personID = ip_personID;
    DELETE FROM person WHERE personID = ip_personID;
END IF;

    
end //

delimiter ;


-- [18] remove_pilot_role()
-- -----------------------------------------------------------------------------
/* This stored procedure removes the pilot role from person.  The pilot must not
be assigned to a flight; or, if they are assigned to a flight, then that flight
must either be at the start or end of its route.  If the person had both a pilot
role and a passenger role, then the person and passenger role data should not be
affected.  If the person only had a pilot role, then all associated person data
must be removed as well. */
-- -----------------------------------------------------------------------------
drop procedure if exists remove_pilot_role;
delimiter //
create procedure remove_pilot_role (in ip_personID varchar(50))
sp_main: begin

DECLARE pilotExists INT DEFAULT 0;
DECLARE pilotFlying INT DEFAULT 0;
DECLARE pilotPassenger INT DEFAULT 0;

-- Check if the pilot exists
SELECT 1 INTO pilotExists FROM pilot WHERE personID = ip_personID;

-- Ensure pilot is not flying
IF pilotExists = 1 THEN
	SELECT 1 INTO pilotFlying FROM pilot 
	WHERE personID = ip_personID AND flying_airline IS NOT NULL;
    
	SELECT 1 INTO pilotPassenger FROM pilot 
	WHERE personID = ip_personID AND flying_tail IN (
		SELECT support_tail FROM flight 
		WHERE routeID IN (SELECT routeID FROM route_path WHERE sequence = 2)
	);
    
	IF pilotFlying = 1 OR pilotPassenger = 1 THEN
		LEAVE sp_main;
	END IF;
END IF;

-- Remove pilot information (Unless passenger)
IF pilotExists = 1 THEN
	IF NOT EXISTS (SELECT 1 FROM passenger WHERE personID = ip_personID) THEN
		DELETE FROM pilot_licenses WHERE personID = ip_personID;
		DELETE FROM pilot WHERE personID = ip_personID;
		DELETE FROM person WHERE personID = ip_personID;
	END IF;
END IF;

end //
delimiter ;


-- [19] flights_in_the_air()
-- -----------------------------------------------------------------------------
/* This view describes where flights that are currently airborne are located. */
-- -----------------------------------------------------------------------------
create or replace view flights_in_the_air (departing_from, arriving_at, num_flights,
	flight_list, earliest_arrival, latest_arrival, airplane_list) AS

SELECT departure AS departing_from, 
       arrival AS arriving_at, 
       COUNT(*) AS num_flights, 
       GROUP_CONCAT(flightID) AS flight_list, 
       MIN(next_time) AS earliest_arrival, 
       MAX(next_time) AS latest_arrival, 
       GROUP_CONCAT(locationID) AS airplane_list
FROM (
    SELECT flits.flightID, 
           roote.departure, 
           roote.arrival, 
           CONCAT(roote.departure, ',', roote.arrival) AS deparr, 
           flits.next_time, 
           airplane.locationID
    FROM flight AS flits
    INNER JOIN (
        SELECT r.routeID, l.legID, r.sequence, l.departure, l.arrival
        FROM route_path AS r
        INNER JOIN leg AS l ON r.legID = l.legID
    ) AS roote ON flits.routeID = roote.routeID AND flits.progress = roote.sequence
    INNER JOIN airplane ON flits.support_tail = airplane.tail_num
    WHERE flits.progress > 0 AND flits.airplane_status LIKE 'in_flight'
) AS s
GROUP BY departure, arrival;



-- [20] flights_on_the_ground()
-- -----------------------------------------------------------------------------
/* This view describes where flights that are currently on the ground are located. */
-- -----------------------------------------------------------------------------
create or replace view flights_on_the_ground (departing_from, num_flights,
	flight_list, earliest_arrival, latest_arrival, airplane_list) as 
    
WITH TempFlights AS (
    SELECT flightID, routeID, progress, locationID, airplane_status, next_time, fr.legID, sequence, distance, departure, arrival
    FROM (
        SELECT flightID, f.routeID, progress, airplane_status, next_time, legID, sequence, locationID 
        FROM (
            SELECT * 
            FROM flight AS fl
            LEFT JOIN airplane AS a ON fl.support_airline = a.airlineID AND fl.support_tail = a.tail_num
        ) AS f
        JOIN route_path AS r ON f.routeID = r.routeID 
        WHERE airplane_status = 'on_ground'
    ) AS fr
    LEFT JOIN leg AS l ON fr.legID = l.legID
)
SELECT
    departing_from,
    COUNT(*) AS num_flights,
    GROUP_CONCAT(flightID) AS flight_list,
    MIN(next_time) AS earliest_arrival,
    MAX(next_time) AS latest_arrival,
    GROUP_CONCAT(locationID SEPARATOR ',') AS airplane_list
FROM (
    SELECT departure AS departing_from, flightID, routeID, progress, locationID, airplane_status, next_time, legID, sequence, distance, arrival
    FROM TempFlights
    WHERE progress = 0 AND sequence = 1

    UNION

    SELECT arrival AS departing_from, flightID, routeID, progress, locationID, airplane_status, next_time, legID, sequence, distance, arrival
    FROM TempFlights
    WHERE progress = sequence
) AS TempFlightsGrouped
GROUP BY departing_from;


-- [21] people_in_the_air()
-- -----------------------------------------------------------------------------
/* This view describes where people who are currently airborne are located. */
-- -----------------------------------------------------------------------------
create or replace view people_in_the_air (departing_from, arriving_at, num_airplanes,
	airplane_list, flight_list, earliest_arrival, latest_arrival, num_pilots,
	num_passengers, joint_pilots_passengers, person_list) as
    
WITH flight_data AS (
    SELECT
        f.flightID,
        l.departure AS departing_from,
        l.arrival AS arriving_at,
        a.locationID AS airplane_location,
        p.personID,
        CASE WHEN pilot.personID IS NOT NULL THEN 1 ELSE 0 END AS is_pilot,
        CASE WHEN passenger.personID IS NOT NULL THEN 1 ELSE 0 END AS isPass,
        f.next_time
    FROM
        flight AS f
    JOIN airplane AS a ON f.support_airline = a.airlineID AND f.support_tail = a.tail_num
    JOIN person AS p ON p.locationID = a.locationID
    JOIN route_path AS r ON f.routeID = r.routeID
    JOIN leg AS l ON r.legID = l.legID
    LEFT JOIN pilot ON p.personID = pilot.personID
    LEFT JOIN passenger ON p.personID = passenger.personID
    WHERE
        airplane_status = 'in_flight' AND f.progress = r.sequence
)
SELECT
    departing_from,
    arriving_at,
    COUNT(DISTINCT airplane_location) AS num_airplane,
    GROUP_CONCAT(DISTINCT airplane_location) AS airplane_list,
    GROUP_CONCAT(DISTINCT flightID) AS flight_list,
    MIN(next_time) AS earliest_arrival,
    MAX(next_time) AS latest_arrival,
    SUM(is_pilot) AS num_pilots,
    SUM(isPass) AS num_passengers,
    COUNT(personID) AS joint_pilots_passengers,
    GROUP_CONCAT(personID) AS person_list
FROM
    flight_data
GROUP BY
    departing_from,
    arriving_at;


-- [22] people_on_the_ground()
-- -----------------------------------------------------------------------------
/* This view describes where people who are currently on the ground are located. */
-- -----------------------------------------------------------------------------
create or replace view people_on_the_ground (departing_from, airport, airport_name,
	city, state, num_pilots, num_passengers, joint_pilots_passengers, person_list) as
    
SELECT 
    a.airportID AS departing_from,
    p.locationID AS airport_locationID,
    a.airport_name,
    a.city,
    a.state,
    COUNT(pilot.personID) AS num_pilots,
    COUNT(passenger.personID) AS num_passengers,
    COUNT(p.personID) AS joint_pilots_passengers,
    GROUP_CONCAT(p.personID) AS person_list
FROM 
    (
        SELECT 
            personID,
            locationID
        FROM 
            person
        WHERE 
            locationID LIKE 'port%'
    ) AS p
JOIN 
    airport AS a ON p.locationID = a.locationID
LEFT JOIN 
    pilot ON p.personID = pilot.personID
LEFT JOIN 
    passenger ON p.personID = passenger.personID
GROUP BY 
    a.airportID, p.locationID, a.airport_name, a.city, a.state
ORDER BY 
    a.airportID;


-- [23] route_summary()
-- -----------------------------------------------------------------------------
/* This view describes how the routes are being utilized by different flights. */
-- -----------------------------------------------------------------------------
create or replace view route_summary (route, num_legs, leg_sequence, route_length,
	num_flights, flight_list, airport_sequence) as
    
WITH temp AS (
    SELECT
        routeID,
        MAX(sequence) AS num_legs,
        GROUP_CONCAT(route_path.legID ORDER BY sequence) AS leg_sequence,
        SUM(distance) AS route_length,
        GROUP_CONCAT(CONCAT(departure, '->', arrival) ORDER BY sequence) AS airport_sequence
    FROM route_path
    JOIN leg ON route_path.legID = leg.legID
    GROUP BY routeID
)
SELECT
    temp.routeID,
    num_legs,
    leg_sequence,
    route_length,
    COUNT(flightID) AS num_flights,
    GROUP_CONCAT(flightID) AS flight_list,
    airport_sequence
FROM temp
LEFT JOIN flight ON temp.routeID = flight.routeID
GROUP BY temp.routeID;



-- [24] alternative_airports()
-- -----------------------------------------------------------------------------
/* This view displays airports that share the same city and state. */
-- -----------------------------------------------------------------------------
create or replace view alternative_airports (city, state, num_airports,
	airport_code_list, airport_name_list) as
    
SELECT
    city,
    state,
    COUNT(*) AS num_airports,
    GROUP_CONCAT(airportID ORDER BY airportID) AS airport_code_list,
    GROUP_CONCAT(airport_name ORDER BY airportID) AS airport_name_list
FROM
    airport
GROUP BY
    city,
    state
HAVING
    num_airports > 1
ORDER BY
    city,
    airport_code_list;

-- [25] simulation_cycle()
-- -----------------------------------------------------------------------------
/* This stored procedure executes the next step in the simulation cycle.  The flight
with the smallest next time in chronological order must be identified and selected.
If multiple flights have the same time, then flights that are landing should be
preferred over flights that are taking off.  Similarly, flights with the lowest
identifier in alphabetical order should also be preferred.

If an airplane is in flight and waiting to land, then the flight should be allowed
to land, passengers allowed to disembark, and the time advanced by one hour until
the next takeoff to allow for preparations.

If an airplane is on the ground and waiting to takeoff, then the passengers should
be allowed to board, and the time should be advanced to represent when the airplane
will land at its next location based on the leg distance and airplane speed.

If an airplane is on the ground and has reached the end of its route, then the
flight crew should be recycled to allow rest, and the flight itself should be
retired from the system. */
-- -----------------------------------------------------------------------------
drop procedure if exists simulation_cycle;
delimiter //
create procedure simulation_cycle ()
sp_main: begin

-- Declare variables
DECLARE flightID_select VARCHAR(100);
DECLARE flightID_select_maxprogress INT;
DECLARE selected_progress VARCHAR(100);

-- Select the flight with the minimum next_time and specific sorting criteria
SELECT flightID INTO flightID_select
FROM flight
WHERE next_time IS NOT NULL
ORDER BY next_time, 
	CASE WHEN airplane_status = 'in_flight' THEN 0 ELSE 1 END,
	CASE WHEN airplane_status = 'in_flight' THEN flightID END,
	flightID ASC
LIMIT 1;

-- Select progress for the selected flight
SELECT progress INTO selected_progress
FROM flight
WHERE flightID = flightID_select;

-- Select the maximum progress from the route_path for the selected flight
SELECT MAX(sequence) INTO flightID_select_maxprogress
FROM flight
NATURAL JOIN route_path
WHERE flightID = flightID_select
GROUP BY flightID;

-- Perform actions based on airplane_status and progress
IF (SELECT airplane_status FROM flight WHERE flightID = flightID_select) = 'in_flight' THEN
    CALL flight_landing(flightID_select);
    CALL passengers_disembark(flightID_select);
    
ELSEIF (SELECT airplane_status FROM flight WHERE flightID = flightID_select) = 'on_ground'
    AND selected_progress < flightID_select_maxprogress THEN
    CALL passengers_board(flightID_select);
    CALL flight_takeoff(flightID_select);
    
ELSEIF (SELECT airplane_status FROM flight WHERE flightID = flightID_select) = 'on_ground'
    AND selected_progress = flightID_select_maxprogress THEN
    CALL recycle_crew(flightID_select);
    CALL retire_flight(flightID_select);
END IF;

end //
delimiter ;
