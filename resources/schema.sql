-- table for local users
CREATE TABLE users (
   id INTEGER primary key,
   username TEXT UNIQUE NOT NULL,      -- username
   password TEXT NOT NULL,             -- password hash + salt
   display_name TEXT,                  -- display name - if null then username
   about TEXT,                         -- about text for user
   pubkey TEXT NOT NULL,               -- public key for user
   privkey TEXT NOT NULL               -- secret key for user
);
CREATE index idxusers_username on users(username);

create table auth (authid integer primary key, userid integer, hash text, expiry text);

CREATE TABLE dream_session (
  id TEXT PRIMARY KEY,
  label TEXT NOT NULL,
  expires_at REAL NOT NULL,
  payload TEXT NOT NULL
);
