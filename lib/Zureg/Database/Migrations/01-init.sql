CREATE TABLE tshirt_sizes (
    tshirt_size TEXT NOT NULL PRIMARY KEY
);

INSERT INTO tshirt_sizes (tshirt_size) VALUES ('XS');
INSERT INTO tshirt_sizes (tshirt_size) VALUES ('S');
INSERT INTO tshirt_sizes (tshirt_size) VALUES ('M');
INSERT INTO tshirt_sizes (tshirt_size) VALUES ('L');
INSERT INTO tshirt_sizes (tshirt_size) VALUES ('XL');
INSERT INTO tshirt_sizes (tshirt_size) VALUES ('XXL');

CREATE TABLE regions (
    region TEXT NOT NULL PRIMARY KEY
);

INSERT INTO regions (region) VALUES ('Switzerland');
INSERT INTO regions (region) VALUES ('Europe');
INSERT INTO regions (region) VALUES ('Africa');
INSERT INTO regions (region) VALUES ('AmericaCentral');
INSERT INTO regions (region) VALUES ('AmericaNorth');
INSERT INTO regions (region) VALUES ('AmericaSouth');
INSERT INTO regions (region) VALUES ('Asia');
INSERT INTO regions (region) VALUES ('MiddleEast');
INSERT INTO regions (region) VALUES ('Oceania');

CREATE TABLE occupations (
    occupation TEXT NOT NULL PRIMARY KEY
);

INSERT INTO occupations (occupation) VALUES ('Student');
INSERT INTO occupations (occupation) VALUES ('Tech');
INSERT INTO occupations (occupation) VALUES ('Academia');
INSERT INTO occupations (occupation) VALUES ('Other');

CREATE TABLE registration_states (
    state TEXT NOT NULL PRIMARY KEY
);

INSERT INTO registration_states (state) VALUES ('Registered');
INSERT INTO registration_states (state) VALUES ('Confirmed');
INSERT INTO registration_states (state) VALUES ('Cancelled');
INSERT INTO registration_states (state) VALUES ('Waitlisted');
INSERT INTO registration_states (state) VALUES ('Spam');

CREATE TABLE registrations (
    id UUID NOT NULL PRIMARY KEY DEFAULT GEN_RANDOM_UUID(),
    name TEXT NOT NULL,
    badge_name TEXT,
    email TEXT NOT NULL,
    affiliation TEXT,
    registered_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    tshirt_size TEXT,
    CONSTRAINT tshirt_size_fk FOREIGN KEY (tshirt_size)
        REFERENCES tshirt_sizes (tshirt_size),
    region TEXT,
    CONSTRAINT region_fk FOREIGN KEY (region) REFERENCES regions (region),
    occupation TEXT,
    CONSTRAINT registrations_occupation_fk FOREIGN KEY (occupation)
        REFERENCES occupations (occupation),
    beginner_track_interest BOOLEAN NOT NULL,
    state TEXT NOT NULL DEFAULT 'Waitlisted',
    CONSTRAINT state_fk FOREIGN KEY (state)
      REFERENCES registration_states (state),
    scanned_at TIMESTAMPTZ,
    vip BOOLEAN NOT NULL DEFAULT false
);

CREATE UNIQUE INDEX registrations_email_idx ON registrations (email);
CREATE INDEX registrations_state_idx ON registrations (state);

CREATE TABLE projects (
    registration_id UUID NOT NULL PRIMARY KEY,
    CONSTRAINT registration_id_fk FOREIGN KEY (registration_id)
        REFERENCES registrations (id),
    name TEXT NOT NULL,
    link TEXT,
    short_description TEXT,
    contributor_level_beginner BOOLEAN NOT NULL,
    contributor_level_intermediate BOOLEAN NOT NULL,
    contributor_level_advanced BOOLEAN NOT NULL
);
