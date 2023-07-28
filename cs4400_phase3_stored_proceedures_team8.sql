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
    
    IF NOT EXISTS (SELECT 1 FROM location WHERE locationID = ip_locationID AND locationID LIKE 'plane%') THEN
        SET airlineExists = 0;
    END IF;
END IF;

IF airlineExists = 0 THEN
    LEAVE sp_main;
END IF;

-- Airplane Insert Statement   
INSERT INTO airplane (airlineID, tail_num, seat_capacity, speed, locationID, plane_type, skids, propellers, jet_engines)
VALUES (ip_airlineID, ip_tail_num, ip_seat_capacity, ip_speed, ip_locationID, ip_plane_type, ip_skids, ip_propellers, ip_jet_engines);

-- Location Insert Statement 
IF ip_locationID IS NOT NULL AND NOT EXISTS (SELECT 1 FROM location WHERE locationID = ip_locationID AND locationID LIKE 'plane%') THEN
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

	-- Airport Uniqe ID
    if ip_airportID is NULL OR ip_airportID in (SELECT airportID FROM airport) then 
		leave sp_main;
	end if;
    
    -- Airport Location ID 
    if (ip_locationID) is NOT NULL AND (ip_locationID) in (SELECT DISTINCT locationID FROM airport) then
		leave sp_main;
	end if;
    
    if (ip_locationID) is NOT NULL AND (ip_locationID) in (SELECT * FROM location WHERE locationID LIKE 'port%') then
		leave sp_main;
	end if;
    
    -- City and State 
    if (ip_city) is NULL OR (ip_state) is NULL then
		leave sp_main;
	end if;
    
    -- Airport Insert Statement 
    insert into airport(airportID, airport_name, city, state, locationID) values (ip_airportID, ip_airport_name, ip_city, ip_state, ip_locationID);
    
    -- Location Insert Statement 
    if (ip_locationID) is NOT NULL AND (ip_locationID) NOT in (SELECT * FROM location WHERE locationID LIKE 'port%') then
		insert into location(locationID) values (ip_locationID);
	end if;

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

	-- Uniqe PersonID 
    if (ip_personID) is NULL OR (ip_personID) in (SELECT personID FROM person) then
		leave sp_main;
	end if;
    
    -- Location Constraints 
    if (ip_locationID) is NULL OR ip_locationID NOT in (SELECT locationID FROM location) then
		leave sp_main;
	end if;
    
    -- Person Insert Statement 
	insert into person(personID, first_name, last_name, locationID) values (ip_personID, ip_first_name, ip_last_name, ip_locationID);
        
	-- Identify passengers and mile constraints 
    if (ip_miles) is NOT NULL then
		insert into passenger(personID, miles) values (ip_personID, ip_miles);
	end if;
    
    -- Identify pilots and taxID constraint as a pilot
    if (ip_taxID) is NOT NULL then
		insert into pilot(personID, taxID, experience, flying_airline, flying_tail) values (ip_personID, ip_taxID, ip_experience, ip_flying_airline, ip_flying_tail);
	end if;
    
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
    if (ip_personID) NOT in (SELECT personID FROM pilot) then
		leave sp_main;
	end if;
    
    -- License is uniqe 
    if (ip_license) in (SELECT license FROM pilot_licenses WHERE personID=ip_personID) then
		leave sp_main;
	end if;

	-- License insert statement 
	insert into pilot_licenses(personID, license) values (ip_personID, ip_license);

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
    if (ip_routeID) NOT in (SELECT routeID FROM route) then
		leave sp_main;
	end if;

	insert into flight(flightID, routeID, support_airline, support_tail, progress, airplane_status, next_time) values (ip_flightID, ip_routeID, ip_support_airline, ip_support_tail, ip_progress, ip_airplane_status, ip_next_time);

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

	-- Flight is tied to valid person, plane must deplane from somewhere, destination airport is listed and same as arrival airport, flight is in the leg of a route 
	if(
    (ip_customer) in (SELECT personID FROM person) AND 
    (ip_carrier) in (SELECT flightID FROM flight) AND 
    (ip_deplane_at) IS NOT NULL AND 
    (ip_deplane_at) NOT IN 
		(SELECT flightID 
		FROM flight 
		WHERE routeID IN 
			(SELECT routeID 
			FROM route_path 
			WHERE legID IN 
				(SELECT legID 
				FROM leg 
                WHERE ip_deplane_at = arrival )))
	)then 
    
		insert ignore into ticket(ticketID, cost, carrier, customer, deplane_at) values (ip_ticketID, ip_cost, ip_carrier, ip_customer, ip_deplane_at);
	end if;
    
    -- Edge Case: Seat number and must be in list of avalibale tickets
	if(ip_seat_number NOT in (SELECT seat_number FROM ticket_seats)) then 
		insert ignore into ticket_seats(ticketID, seat_number) values (ip_ticketID, ip_seat_number);
	end if;
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

	-- Checking if leg already exists
    if exists (SELECT * FROM leg WHERE departure = ip_departure AND arrival=ip_arrival) then
    
	-- Update current leg while keeping id
        update leg set distance = ip_distance WHERE (departure = ip_departure) and (arrival = ip_arrival);
        
	-- Case that leg dosent exist 
	else
	-- Leg insert statement 
        insert into leg(legID, distance, departure, arrival) values (ip_legID, ip_distance, ip_departure, ip_arrival);
    end if;
    
    -- Checking for leg in opposite direction 
    if exists (SELECT * FROM leg WHERE (departure = ip_arrival) AND (arrival = ip_departure)) then
    
	-- Making sure distance is equivilent 
        update (leg) set distance = ip_distance WHERE (departure = ip_arrival) AND (arrival = ip_departure);
	end if;

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

	-- Route insert statement 
	insert into route(routeID) values (ip_routeID);
    
    -- Path insert statement
    insert into route_path(routeID, legID, sequence) values (ip_routeID, ip_legID, 1);
    
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

if (ip_legID) IS NULL OR (ip_routeID) IS NULL then leave sp_main; 
end if;

-- Finding max of sequence 
set @max_sequence = (SELECT max(sequence) FROM (route_path) WHERE (ip_routeID = routeID) GROUP BY (routeID));

-- Departure airport 
set @new_leg_departure = (SELECT (departure) FROM (leg) WHERE (ip_legID = legID));

-- Identifying leg 
set @leg = (SELECT (legID) FROM (route_path) WHERE (ip_routeID = routeID) AND (@max_sequence = sequence));
if @leg IS NULL then leave sp_main; 
end if;

-- Arrival airport must be departure airport 
set @route_arrival = (SELECT arrival FROM leg WHERE @leg = legID);
if @route_arrival != @new_leg_departure then leave sp_main; 
end if;

-- Inserting leg into route if not already in it 
if ip_legID not in (SELECT legID FROM route_path) then insert into route_path values (ip_routeID, ip_legID, @max_sequence + 1); 
end if;


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

-- Setting initial distance to zero 
	declare distance int default 0;

-- Adding one hour to flight time & grounding flight status
	if (ip_flightID) in (SELECT flightID FROM flight) then 
		UPDATE flight SET airplane_status = 'on_ground', next_time = ADDTIME(next_time, '01:00:00') WHERE (flightID = ip_flightID);
	end if; 

-- Increasing pilot expirience 
	if (ip_flightID) in (SELECT flightID FROM flight WHERE support_tail in (SELECT flying_tail FROM pilot)) then
		UPDATE pilot SET experience = experience + 1 WHERE personID in (SELECT * FROM (SELECT p.personID FROM flight AS f JOIN pilot AS p ON f.support_airline=p.flying_airline AND f.support_tail=p.flying_tail WHERE flightID = ip_flightID) AS temp);
	end if;
    
-- Updating passanger frequency
	set distance = (SELECT l.distance FROM flight AS f JOIN route_path AS r ON f.routeID=r.routeID JOIN leg AS l ON r.legID=l.legID WHERE f.flightID=ip_flightID AND f.progress=r.sequence);
	UPDATE passenger
	SET miles = miles + distance
	WHERE personID in (SELECT t.customer FROM ticket AS t JOIN flight AS f ON t.carrier=f.flightID WHERE t.carrier = ip_flightID);


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

-- Setting initial duration to zero 
	declare next_duration int default 0;
    
-- Constraint: Flight must exist 
	if (ip_flightID NOT in (SELECT flightID FROM flight)) then
		leave sp_main;
	end if;
    
-- Checking for piolit shortage - jet 
	if 
    (SELECT plane_type 
    FROM flight AS f 
    JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num 
    WHERE f.flightID=ip_flightID) = 'jet' and 
		(SELECT COUNT(p.personID) 
        FROM flight AS f 
        JOIN pilot AS p on f.support_airline=p.flying_airline AND f.support_tail=p.flying_tail 
        WHERE f.flightID=ip_flightID)<2 
	then
        
-- If shortage then delay flight
        update flight set next_time=next_time + interval 30 minute WHERE flightID=ip_flightID;
        leave sp_main;
	end if;
    
-- Checking for piolit shortage - propeller 
    if 
    (SELECT plane_type 
    FROM flight AS f 
    JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num 
    WHERE f.flightID=ip_flightID) = 'prop' AND 
		(SELECT COUNT(p.personID) 
		FROM flight AS f 
        JOIN pilot AS p on f.support_airline=p.flying_airline AND f.support_tail=p.flying_tail 
        WHERE f.flightID=ip_flightID)<1 
	then
        
 -- If shortage delay flight
        update flight set next_time=next_time + interval 30 minute WHERE flightID=ip_flightID;
        leave sp_main;
	end if;

-- Shortages accounted for, update flight progress
    update flight set progress=progress+1 WHERE flightID=ip_flightID;
    
-- Flight progress updated, update airplane status 
    update flight set airplane_status='in_flight' WHERE flightID=ip_flightID;
    
-- Find flight duration 
    set next_duration = (SELECT l.distance/a.speed FROM flight AS f JOIN route_path AS r on f.routeID=r.routeID AND f.progress=r.sequence JOIN leg AS l on r.legID=l.legID JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num WHERE f.flightID=ip_flightID);
    
-- Next flight updated
    update flight set next_time = next_time + interval next_duration hour WHERE flight.flightID=ip_flightID;

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
	declare board_plane_loc varchar(10);
    
-- Constraint: Flight must exist
	if (ip_flightID NOT in (SELECT flightID FROM flight)) then
		leave sp_main;
	end if;
    
  -- Passanger location ID must match departing airport, passanger must have ticket 
    if exists
		(SELECT * 
		FROM flight AS f 
        JOIN ticket AS t on t.carrier=f.flightID 
        JOIN person AS p on t.customer=p.personID 
        WHERE flightID=ip_flightID AND p.locationID = 
			(SELECT a.locationID
            FROM flight AS f 
            JOIN route_path AS r on r.routeID=f.routeID 
            JOIN leg AS l on r.legID=l.legID 
            JOIN airport AS a on l.departure=a.airportID 
            WHERE flightID=ip_flightID AND f.progress+1=r.sequence)) then 
                    
-- Update boarding location 
        set board_plane_loc = (SELECT a.locationID FROM flight AS f JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num WHERE flightID=ip_flightID);
        
-- Update person LocationID
        update person 
			set locationID = board_plane_loc 
			WHERE personID in 
				(SELECT * 
                FROM 
					(SELECT t.customer 
                    FROM flight AS f 
                    JOIN ticket AS t on t.carrier=f.flightID
                    JOIN person AS p on t.customer=p.personID 
                    WHERE flightID=ip_flightID AND p.locationID = 
						(SELECT a.locationID 
						FROM flight AS f 
                        JOIN route_path AS r on r.routeID=f.routeID 
                        JOIN leg AS l on r.legID=l.legID 
                        JOIN airport AS a on l.departure=a.airportID 
                        WHERE flightID=ip_flightID AND f.progress+1=r.sequence)) AS temp);
		end if;
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

	declare deplane_loc varchar(10);
    
-- Checking is passangers are on flight, that flight is at the destination airport, and Edge case that flight is on the ground
-- Updating plane location if constraints are met 
	if exists 
    (SELECT * 
    FROM flight AS f 
    JOIN ticket AS t on t.carrier = f.flightID 
    JOIN person AS p on t.customer=p.personID 
    JOIN route_path AS r on f.routeID = r.routeID and f.progress=r.sequence 
    JOIN leg AS l on r.legID=l.legID
	WHERE flightID = ip_flightID AND airplane_status='on_ground' AND p.locationID = 
		(SELECT locationID 
        FROM flight AS f 
        JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num 
        WHERE flightID = ip_flightID) AND t.deplane_at=l.arrival) then

        set deplane_loc = 
        (SELECT DISTINCT a.locationID 
        FROM flight AS f 
        JOIN ticket AS t on t.carrier = f.flightID 
        JOIN route_path AS r on f.routeID = r.routeID AND f.progress=r.sequence 
        JOIN leg AS l on r.legID=l.legID JOIN airport AS a on a.airportID=l.arrival 
        WHERE flightID =ip_flightID AND t.deplane_at=l.arrival);
        
-- update person locationID 
        update person set locationID = deplane_loc
        WHERE personID in 
			(SELECT * 
            FROM 
				(SELECT t.customer 
                FROM flight AS f 
                JOIN ticket AS t on t.carrier = f.flightID 
                JOIN person AS p on t.customer=p.personID 
                JOIN route_path AS r on f.routeID = r.routeID AND f.progress=r.sequence 
                JOIN leg AS l on r.legID=l.legID 
                WHERE flightID =ip_flightID AND airplane_status='on_ground' and p.locationID = 
					(SELECT locationID 
					FROM flight AS f 
                    JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num 
                    WHERE flightID = ip_flightID) AND t.deplane_at=l.arrival) AS temp);               
	end if;
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

-- Setting data types 
	declare plane_type varchar(10);
    declare pilot_loc varchar(10);

-- Assighned piolit must have a licence 
    set plane_type = 
    (SELECT a.plane_type FROM flight AS f JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num WHERE flightID=ip_flightID);
    
    if plane_type NOT in (SELECT license FROM pilot AS p JOIN pilot_licenses AS pl on p.personID=pl.personID WHERE p.personID=ip_personID) then
		leave sp_main;
	end if;
    
-- Edge case, pilot and plane location must match 
    set pilot_loc = (SELECT locationID FROM pilot AS pil JOIN person AS per on pil.personID=per.personID WHERE pil.personID=ip_personID);
    if pilot_loc != (SELECT a.locationID FROM flight AS f JOIN route_path AS r on r.routeID=f.routeID JOIN leg AS l on r.legID=l.legID JOIN airport AS a on l.arrival=a.airportID WHERE flightID=ip_flightID AND f.progress=r.sequence) then
		leave sp_main;
	end if;
    
-- Edge case, pilot is only on one flight
    if (SELECT flying_airline FROM pilot WHERE personID=ip_personID) is NOT NULL or (SELECT flying_tail FROM pilot WHERE personID=ip_personID) is NOT NULL then
		leave sp_main;
	end if;
    
-- Update piolt tables 
	update pilot set flying_airline=(SELECT support_airline FROM flight WHERE flightID=ip_flightID) WHERE personID=ip_personID;
	update pilot set flying_tail=(SELECT support_tail FROM flight WHERE flightID=ip_flightID) WHERE personID=ip_personID;
    update person set locationID = (SELECT a.locationID FROM flight AS f JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num WHERE flightID=ip_flightID) WHERE personID=ip_personID;

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

-- Setting data types
	declare end_leg int default 0;
    declare arrival_loc varchar(10);
    declare total_p int default 0;
    declare disembarked_p int default 0;

-- Flight progress check 
    set end_leg = (SELECT max(r.sequence) FROM flight AS f JOIN route_path AS r on f.routeID=r.routeID WHERE flightID=ip_flightID);
	if end_leg != (SELECT progress FROM flight WHERE flightID=ip_flightID) then
		leave sp_main;
	end if;
    
-- Grounded flight check 
    if ip_flightID not in (SELECT flightID FROM flight WHERE airplane_status='on_ground') then
		leave sp_main;
	end if;
    
-- Passenger disembark at destination airport check
    set arrival_loc = (SELECT a.locationID FROM flight AS f JOIN route_path AS r on f.routeID=r.routeID JOIN leg AS l on r.legID=l.legID JOIN airport AS a on l.arrival=a.airportID WHERE f.flightID=ip_flightID AND f.progress=r.sequence);
    
-- Total passanger count 
    set total_p = (SELECT count(t.customer) FROM flight AS f JOIN ticket AS t on f.flightID=t.carrier WHERE f.flightID=ip_flightID);
    
-- Disembarked passenger count
    set disembarked_p = (SELECT count(t.customer) FROM flight AS f JOIN ticket AS t on f.flightID=t.carrier JOIN person AS p on t.customer=p.personID WHERE f.flightID=ip_flightID and p.locationID=arrival_loc);
		
	if total_p != disembarked_p then
		leave sp_main;
	end if;
    
-- Update pilot location 
    update person set locationID=arrival_loc WHERE personID in (SELECT personID FROM pilot AS p JOIN flight AS f on f.support_airline=p.flying_airline AND f.support_tail=p.flying_tail WHERE f.flightID=ip_flightID);
    
-- Update airplane adn airline 
    update pilot set flying_airline = NULL, flying_tail = NULL WHERE personID in (SELECT * FROM (SELECT personID FROM pilot AS p JOIN flight AS f on f.support_airline=p.flying_airline AND f.support_tail=p.flying_tail WHERE f.flightID=ip_flightID) AS temp);

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

-- Set variable types 
	declare end_leg int default 0;
    
-- Flight is grounded
    if ip_flightID NOT in (SELECT flightID FROM flight WHERE airplane_status='on_ground') then 
		leave sp_main; 
	end if;
    
-- Flight is at destination or arival 
    set end_leg = (SELECT max(r.sequence) FROM flight AS f JOIN route_path AS r on f.routeID=r.routeID WHERE f.flightID=ip_flightID);
   
    if ip_flightID NOT in (SELECT flightID FROM flight WHERE progress=0 OR progress=end_leg) then
		leave sp_main;
	end if;

-- Retire flight 
    delete from flight WHERE flightID = ip_flightID;
    
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

-- Edge case: Passanger must exist 
    if (ip_personID NOT in (SELECT personID FROM passenger)) then
		leave sp_main;
    end if;
    
-- Passanger is on ground 
    if (ip_personID in (SELECT personID FROM passenger WHERE personID in (SELECT personID FROM person WHERE locationID LIKE '%plane%'))) then 
		leave sp_main;
    end if;
    
-- Remove passenger information (except for pilot)
    if (ip_personID NOT in (SELECT personID FROM pilot)) then
        DELETE FROM passenger WHERE ip_personID = personID;
        DELETE FROM person WHERE ip_personID = personID;
    end if;
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

-- Edge case: Pilot must exist 
    if (ip_personID NOT in (SELECT personID FROM pilot)) then
		leave sp_main;
    end if;
    
-- Ensure pilot is not flying 
    if (ip_personID in (SELECT flying_airline FROM pilot) OR ip_personID in ((SELECT flying_airline FROM pilot WHERE flying_tail in(SELECT support_tail FROM flight WHERE routeID in (SELECT routeID FROM route_path WHERE sequence=2))))) then 
		leave sp_main;
    end if;
    
-- Remove pilot information (Unless passanger)
    if (ip_personID NOT in (SELECT personID FROM passenger)) then
		DELETE FROM pilot_licenses WHERE ip_personID = personID;
        DELETE FROM pilot WHERE ip_personID = personID;
        DELETE FROM person WHERE ip_personID = personID;
    end if;
end //
delimiter ;


-- [19] flights_in_the_air()
-- -----------------------------------------------------------------------------
/* This view describes where flights that are currently airborne are located. */
-- -----------------------------------------------------------------------------
create or replace view flights_in_the_air (departing_from, arriving_at, num_flights,
	flight_list, earliest_arrival, latest_arrival, airplane_list) AS
    
    SELECT l.departure, l.arrival, count(f.flightID), group_concat(f.flightID), min(f.next_time), max(f.next_time), group_concat(a.locationID)
	FROM flight AS f
	JOIN route_path AS r on f.routeID=r.routeiD
	JOIN leg AS l on r.legID=l.legID
	JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num
	WHERE airplane_status='in_flight' AND f.progress=r.sequence
	GROUP BY l.departure, l.arrival;
SELECT NULL, NULL, 0, NULL, NULL, NULL, NULL;

-- [20] flights_on_the_ground()
-- -----------------------------------------------------------------------------
/* This view describes where flights that are currently on the ground are located. */
-- -----------------------------------------------------------------------------
create or replace view flights_on_the_ground (departing_from, num_flights,
	flight_list, earliest_arrival, latest_arrival, airplane_list) as 
    
    SELECT l.departure AS departing_from, count(f.flightID) AS num_flights, group_concat(flightID) AS flight_list, min(f.next_time) AS earliest_arrival, max(f.next_time) AS latest_arrival, group_concat(a.locationID) AS airplane_list
	FROM flight AS f 
    JOIN route_path AS r on f.routeID=r.routeID
	JOIN leg AS l on r.legID=l.legID
	JOIN airplane AS a on f.support_airline=a.airlineID AND f.support_tail=a.tail_num
	WHERE airplane_status='on_ground' AND f.progress+1=r.sequence
	GROUP BY l.departure
	ORDER BY flight_list;
SELECT NULL, 0, NULL, NULL, NULL, NULL;

-- [21] people_in_the_air()
-- -----------------------------------------------------------------------------
/* This view describes where people who are currently airborne are located. */
-- -----------------------------------------------------------------------------
create or replace view people_in_the_air (departing_from, arriving_at, num_airplanes,
	airplane_list, flight_list, earliest_arrival, latest_arrival, num_pilots,
	num_passengers, joint_pilots_passengers, person_list) as
    
    SELECT l.departure AS departing_from, l.arrival AS arriving_at, count(DISTINCT a.locationID) AS num_airplane, group_concat(DISTINCT a.locationID) AS airplane_list, group_concat(DISTINCT f.flightID) AS flight_list, min(f.next_time) AS earliest_arrival, max(f.next_time) AS latest_arrival, count(pilot.personID) AS num_pilots, count(passenger.personID) AS num_passengers, count(p.personID) AS joint_pilots_passengers, group_concat(p.personID) AS person_list
	FROM flight AS f
	JOIN airplane AS a on f.support_airline=a.airlineID and f.support_tail=a.tail_num
	JOIN person AS p on p.locationID=a.locationID
	JOIN route_path AS r on f.routeID=r.routeiD
	JOIN leg AS l on r.legID=l.legID
	LEFT JOIN pilot on p.personID=pilot.personID
	LEFT JOIN passenger on p.personID=passenger.personID
	WHERE airplane_status='in_flight' AND f.progress=r.sequence
	GROUP BY l.departure, l.arrival;
SELECT NULL, NULL, 0, NULL, NULL, NULL, NULL, 0, 0, NULL, NULL;

-- [22] people_on_the_ground()
-- -----------------------------------------------------------------------------
/* This view describes where people who are currently on the ground are located. */
-- -----------------------------------------------------------------------------
create or replace view people_on_the_ground (departing_from, airport, airport_name,
	city, state, num_pilots, num_passengers, joint_pilots_passengers, person_list) as
    
	SELECT a.airportID AS departing_from, p.locationID AS airport, a.airport_name, a.city, a.state,count(pilot.personID) AS num_pilots, count(passenger.personID) AS num_passengers, count(p.personID) AS joint_pilots_passengers, group_concat(p.personID) AS person_list
	FROM person AS p
    JOIN airport AS a on p.locationID=a.locationID
	LEFT JOIN pilot on p.personID=pilot.personID
	LEFT JOIN passenger on p.personID=passenger.personID
	WHERE p.locationID LIKE 'port%'
	GROUP BY a.airportID, p.locationID, a.airport_name, a.city, a.state
	ORDER BY a.airportID;
SELECT NULL, NULL, NULL, NULL, NULL, 0, 0, NULL, NULL;

-- [23] route_summary()
-- -----------------------------------------------------------------------------
/* This view describes how the routes are being utilized by different flights. */
-- -----------------------------------------------------------------------------
create or replace view route_summary (route, num_legs, leg_sequence, route_length,
	num_flights, flight_list, airport_sequence) as
    
    SELECT r.routeID AS route, count(DISTINCT r.legID) AS num_legs, group_concat(DISTINCT r.legID ORDER BY sequence) AS leg_sequence, 
			cast((case when count(DISTINCT flightID)>1 then sum(distance)/count(DISTINCT flightID) else sum(distance) end) AS decimal) AS route_length,
			count(distinct flightID) AS num_flights, group_concat(distinct flightID) AS flight_list, group_concat(DISTINCT concat(departure, '->', arrival) ORDER BY sequence) AS airport_sequence
	FROM route_path AS r 
	JOIN leg AS l on r.legID = l.legID 
	LEFT JOIN flight AS f on r.routeID = f.routeID
	GROUP BY r.routeID, f.routeID;
SELECT NULL, 0, NULL, 0, 0, NULL, NULL;

-- [24] alternative_airports()
-- -----------------------------------------------------------------------------
/* This view displays airports that share the same city and state. */
-- -----------------------------------------------------------------------------
create or replace view alternative_airports (city, state, num_airports,
	airport_code_list, airport_name_list) as
    
    SELECT city, state, (case when count(*)>1 then count(*) else count(*) end) AS num_airports,
			group_concat(airportID ORDER BY airportID) AS airport_code_list, group_concat(airport_name ORDER BY airportID) AS airport_name_list
	FROM airport
	GROUP BY city, state
	HAVING num_airports>1
	ORDER BY city, airport_code_list;

SELECT NULL, NULL, 0, NULL, NULL;

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

-- Next flight in cycle 
	set @next_flight = (SELECT  flightID FROM flight WHERE next_time = (SELECT min(next_time) FROM flight) GROUP BY flightID ORDER BY airplane_status = 'in_flight', flightID ASC limit 1);

-- Flight landing
	IF EXISTS (SELECT flightID FROM flight WHERE airplane_status = 'in_flight' AND flightID = @next_flight) then
	update flight set airplane_status = 'on_ground' WHERE flightID = @next_flight;

	set @new_next_flight = (SELECT flightID FROM flight WHERE next_time > (SELECT next_time FROM flight WHERE flightID = @next_flight) GROUP BY flightID ORDER BY airplane_status = 'in_flight', flightID ASC limit 1);
	set @new_time = DATE_SUB((SELECT next_time FROM flight WHERE flightID = @new_next_flight), interval 1 hour);

	update flight set next_time = @new_time WHERE flightID = @next_flight;

end if;

-- Flight takeoff
	if exists (SELECT flightID FROM flight WHERE airplane_status = 'on_ground' AND flightID = @next_flight) then

-- Flight progress 
	update flight set airplane_status = 'in_flight' WHERE flightID = @next_flight;
	update flight set progress = progress + 1 WHERE flightID = @next_flight;

	set @calculated_dist = (SELECT distance FROM leg l, airplane a, flight f, route_path rp WHERE f.flightID = @next_flight AND f.routeID = rp.routeID AND f.support_tail = a.tail_num AND rp.legID = l.legID);
	set @calculated_speed = (SELECT speed FROM leg l, airplane a, flight f, route_path rp WHERE f.flightID = @next_flight AND f.routeID = rp.routeID AND f.support_tail = a.tail_num AND rp.legID = l.legID);
	set @calculated_time = @calculated_dist/@calculated_speed;

	update flight set next_time = DATE_ADD(next_time, interval @calculated_time hour) WHERE flightID = @next_flight;
    
end if;

-- Flight ended
	if exists(SELECT f.flightID FROM flight f, route_path rp WHERE f.airplane_status = 'on_ground' AND f.routeID = rp.routeID AND f.flightID = @next_flight AND f.progress = rp.sequence) then DELETE FROM flight WHERE flightID = @next_flight;

end if;

end //
delimiter ;

