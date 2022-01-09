CREATE TABLE rover(
    id serial8 PRIMARY KEY,
    name VARCHAR(200) UNIQUE NOT NULL,
    landing_date date NOT NULL,
    launch_date date NOT NULL,
    status text NOT NULL,
    max_sol int8 NOT NULL,
    max_date date NOT NULL
);

CREATE TABLE photo(
    id serial8 PRIMARY KEY,
    rover_id int8 REFERENCES rover (id) ON DELETE CASCADE,
    camera_name text NOT NULL,
    img_src text UNIQUE NOT NULL,
    earth_date date NOT NULL
);

  -- One to One table with rovers
CREATE TABLE job(
    rover_id int8 UNIQUE REFERENCES rover (id) ON DELETE CASCADE,
    sol int8 NOT NULL,
    page int8 NOT NULL,
    PRIMARY KEY (rover_id)
);
