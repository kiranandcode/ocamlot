PRAGMA foreign_keys = ON;

-- dream configuration
create table auth (authid integer primary key, userid integer, hash text, expiry text);

CREATE TABLE dream_session (
  id TEXT PRIMARY KEY,
  label TEXT NOT NULL,
  expires_at REAL NOT NULL,
  payload TEXT NOT NULL
);

-- table for local users
CREATE TABLE LocalUser (
   id INTEGER PRIMARY KEY,
   username TEXT UNIQUE NOT NULL,      -- username
   password TEXT NOT NULL,             -- password hash + salt
   display_name TEXT,                  -- display name - if null then username
   about TEXT,                         -- about text for user
   pubkey TEXT NOT NULL,               -- public key for user
   privkey TEXT NOT NULL               -- secret key for user
);
CREATE index idxLocalUser_username on LocalUser(username);

-- table for remote instances
CREATE TABLE RemoteInstance (
   id INTEGER PRIMARY KEY,             -- internal id used for keeping track of remote instances
   url  string NOT NULL,               -- url of instance
   last_unreachable  TEXT              -- timestamp of the last time the instance was unreachable, null if never unreachable
);


-- table for remote users
CREATE TABLE RemoteUser (
   id INTEGER PRIMARY KEY,             -- internal id used for keeping track of remote users
   username TEXT UNIQUE NOT NULL,      -- username
   instance_id INTEGER NOT NULL,       -- instance of user
   display_name TEXT,                  -- display name - if null then username
   url  TEXT,                          -- url of actor, if not present, then username @ instance_id.url
   raw_data  TEXT,                      -- raw json object of user
   FOREIGN KEY (instance_id)
       REFERENCES RemoteInstance (id)
       ON UPDATE RESTRICT
       ON DELETE RESTRICT
);

-- local/remote independent table encoding of actors, either local_id or remote_id will be null
CREATE TABLE Actor (
   id  INTEGER PRIMARY KEY,            -- internal id for referring to actors
   local_id  INTEGER,                  -- local id if a local user
   remote_id  INTEGER,                 -- remote id if a local user
   FOREIGN KEY (local_id)
      REFERENCES LocalUser (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
   FOREIGN KEY (remote_id)
      REFERENCES RemoteUser (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);

-- table for posts
CREATE TABLE Post (
   id INTEGER PRIMARY KEY,           -- internal post id, not exposed
   public_id STRING,                 -- if post is by an local user, then assign a public id for the url
   url STRING NOT NULL,              -- url of the post, if local, then /api/posts/<public_id>
   author_id INTEGER,                -- author of the post

   raw_data TEXT,                    -- if by an external user, then keep raw json of the post
   FOREIGN KEY (author_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);
CREATE index idxPost_public_id on Post(public_id);

-- table for mentions
CREATE TABLE Mentions (
   public_id STRING,                 -- if mention is by a local user, then assign a public id for the url (TODO, do we need this?)
   url STRING NOT NULL,              -- url of the mention, if local, then /api/mentions/<public_id>
   raw_data TEXT,                    -- if by an external user, then keep raw json of the mention

   post_id INTEGER NOT NULL,        -- post doing the mentioning
   actor_id INTEGER NOT NULL,       -- actor being mentioned

   PRIMARY KEY (post_id, actor_id)

   FOREIGN KEY (post_id)
      REFERENCES Post (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
   FOREIGN KEY (actor_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT

);
CREATE index idxMentions_public_id on Mentions(public_id);


-- table for likes
CREATE TABLE Likes (
   id INTEGER PRIMARY KEY,         -- internal like id, not exposed

   post_id INTEGER NOT NULL,       -- post being liked
   actor_id INTEGER NOT NULL,      -- actor doing the liking


   public_id STRING,               -- if like by a local user, then assign a public id for the like object
   url STRING NOT NULL,            -- url of the like object, if local, then /api/likes/<public_id>
   raw_data TEXT,                  -- if by an external user, then keep the raw json of the like object

   FOREIGN KEY (post_id)
      REFERENCES Post (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
   FOREIGN KEY (actor_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);
CREATE index idxLikes_public_id on Likes(public_id);


-- table for follows 
CREATE TABLE Follows (
   id INTEGER PRIMARY KEY,          -- internal like id, not exposed
   public_id STRING,                -- if follow by a local user, then assign a public id for the like object
   url STRING,                      -- url of the follow object, if local then /api/follows/<public_id>
   raw_data TEXT,                  -- if by an external user, then keep the raw json of the like object

   author_id INTEGER NOT NULL,           -- id of the author of the follow
   target_id INTEGER NOT NULL,           -- id of the actor being followed

   FOREIGN KEY (author_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
   FOREIGN KEY (target_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);
CREATE index idxFollows_public_id on Follows(public_id);
