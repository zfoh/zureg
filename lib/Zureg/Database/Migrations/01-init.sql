CREATE TABLE registrants (
    id UUID NOT NULL PRIMARY KEY DEFAULT GEN_RANDOM_UUID(),
    email TEXT NOT NULL,
    name TEXT NOT NULL,
    badge_name TEXT,
    affiliation TEXT,
    registered_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX registrants_email_idx ON registrants (email);
