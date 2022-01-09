/*
New camera schema
 */
CREATE TABLE IF NOT EXISTS camera (
    id serial8 PRIMARY KEY,
    rover_id int8 REFERENCES rover (id) ON DELETE CASCADE,
    name text NOT NULL,
    UNIQUE (rover_id, name)
);


-- new field on photo, camera_id
ALTER TABLE photo     
    ADD COLUMN IF NOT EXISTS camera_id int8
;


-- migrate camera table
INSERT INTO 
    camera (rover_id, name)
    (
        SELECT DISTINCT 
            rover_id, 
            camera_name AS name
        FROM 
            photo
    )
ON CONFLICT DO NOTHING;


-- migrate photo table
UPDATE 
    photo
SET
    camera_id = camera.id 
FROM 
    camera
WHERE
    camera.name = photo.camera_name
;

-- cleanup photo table
ALTER TABLE photo
    DROP COLUMN camera_name,
    ALTER COLUMN camera_id SET NOT NULL,
    ADD CONSTRAINT photo_camera_fk FOREIGN KEY (camera_id) REFERENCES camera (id) MATCH FULL;
;
