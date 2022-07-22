PRAGMA foreign_keys = ON;

-- dream configuration
CREATE TABLE dream_session (
  id TEXT PRIMARY KEY,
  label TEXT NOT NULL,
  expires_at REAL NOT NULL /* date */,
  payload TEXT NOT NULL
);

CREATE TABLE Activity (
  id TEXT PRIMARY KEY /* Uuidm.t */,                 -- uuid of the activity
  raw_data BLOB NOT NULL /* data: Yojson.Safe.t */        -- json data
);

-- table for local users
CREATE TABLE LocalUser (
   id INTEGER PRIMARY KEY,
   username TEXT UNIQUE NOT NULL,                        -- username
   password TEXT NOT NULL /* password_hash: string */,   -- password hash + salt
   display_name TEXT,                                    -- display name - if null then username
   about TEXT,                                           -- about text for user
   manually_accept_follows BOOLEAN NOT NULL,             -- whether the user is an admin
   is_admin BOOLEAN NOT NULL,                            -- whether the user is an admin

   pubkey TEXT NOT NULL /* X509.Public_key.t */,         -- public key for user
   privkey TEXT NOT NULL /* X509.Private_key.t */        -- secret key for user
);
CREATE index idxLocalUser_username on LocalUser(username);

-- table for remote instances
CREATE TABLE RemoteInstance (
   id INTEGER PRIMARY KEY,                             -- internal id used for keeping track of remote instances
   url TEXT NOT NULL UNIQUE,                           -- url of instance
   last_unreachable  TEXT /* Calendar.t option */      -- timestamp of the last time the instance was unreachable, null if never unreachable
);
CREATE index idxRemoteInstance_url on RemoteInstance(url);

-- table for remote users
CREATE TABLE RemoteUser (
   id INTEGER PRIMARY KEY,             -- internal id used for keeping track of remote users
   username TEXT UNIQUE NOT NULL,      -- username
   instance_id INTEGER NOT NULL,       -- instance of user
   display_name TEXT,                  -- display name - if null then username
   url  TEXT UNIQUE NOT NULL,          -- url of actor (obtained via webfinger if needed)

   inbox TEXT,                         -- inbox url of the user
   outbox TEXT,                        -- outbox url of the user

   followers TEXT,                     -- followers url of the user
   following TEXT,                     -- following url of the user

   summary TEXT,                       -- summary string of the user
   public_key_pem TEXT NOT NULL,                -- public key of the user
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
CREATE TABLE Posts (
   id INTEGER PRIMARY KEY,                             -- internal post id, not exposed
   public_id TEXT,                                     -- if post is by an local user, then assign a public id for the url
   url TEXT NOT NULL UNIQUE,                           -- url/id of the post, if local, then /api/posts/<public_id>
   author_id INTEGER NOT NULL /* author: int64 */,                         -- author of the post

   is_public BOOLEAN NOT NULL,                                  -- is the post public? or only to the mentioned users

   summary TEXT,                                       -- subject of the post
   post_source TEXT NOT NULL,                          -- source of the post

   published TEXT NOT NULL /* Calendar.t */,           -- date at which post was published

   raw_data BLOB /* raw_text: string option */,        -- if by an external user, then keep raw json of the post
   FOREIGN KEY (author_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);
CREATE index idxPost_public_id on Posts(public_id);

-- table for post-to value
CREATE TABLE PostTo (
   post_id INTEGER NOT NULL,                  -- post in question
   actor_id INTEGER NOT NULL,                 -- target of the post

   PRIMARY KEY (post_id, actor_id)

   FOREIGN KEY (post_id)
      REFERENCES Posts (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT

   FOREIGN KEY (actor_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);

-- table for post-cc value
CREATE TABLE PostCc (
   post_id INTEGER NOT NULL,                  -- post in question
   actor_id INTEGER NOT NULL,                -- target of the post

   PRIMARY KEY (post_id, actor_id)

   FOREIGN KEY (actor_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT

   FOREIGN KEY (post_id)
      REFERENCES Posts (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);

-- table for mentions
CREATE TABLE Mention (

   post_id INTEGER NOT NULL,        -- post doing the mentioning
   actor_id INTEGER NOT NULL,       -- actor being mentioned

   PRIMARY KEY (post_id, actor_id)

   FOREIGN KEY (post_id)
      REFERENCES Posts (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
   FOREIGN KEY (actor_id)
      REFERENCES Actor (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT

);

-- table for Tags
CREATE TABLE Tags (
   tag_id INTEGER PRIMARY KEY /* id: int64 */,             -- tag id
   tag_name TEXT NOT NULL UNIQUE /* name: string */        -- tag name
);
CREATE index idxTags_tag_name on Tags(tag_name);

-- table for PostTags (references to tags in posts)
CREATE TABLE PostTags (
   post_id INTEGER,                  -- post id
   tag_id INTEGER,                   -- tag id
   url TEXT,                         -- href of the tag root (i.e head to url to see all posts) if external
   PRIMARY KEY (post_id, tag_id)
   FOREIGN KEY (post_id)
      REFERENCES Posts (id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
   FOREIGN KEY (tag_id)
      REFERENCES Tags (tag_id)
      ON UPDATE RESTRICT
      ON DELETE RESTRICT
);
CREATE index idxPostTags_tag_url on PostTags(url);

-- table for likes
CREATE TABLE Likes (
   id INTEGER PRIMARY KEY,         -- internal like id, not exposed

   post_id INTEGER NOT NULL,       -- post being liked
   actor_id INTEGER NOT NULL,      -- actor doing the liking

   published DATE NOT NULL /* CalendarLib.Calendar.t */,        -- date at which like was published

   public_id TEXT,                 -- if like by a local user, then assign a public id for the like object
   url TEXT NOT NULL,              -- url of the like object, if local, then /api/likes/<public_id>
   raw_data BLOB,                  -- if by an external user, then
                                   -- keep the raw json of the like
                                   -- object

   FOREIGN KEY (post_id)
      REFERENCES Posts (id)
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
   id INTEGER PRIMARY KEY,               -- internal like id, not exposed
   public_id TEXT,                       -- if follow by a local user, then assign a public id for the follow object
   url TEXT NOT NULL,                             -- url of the follow object, if local then /api/follows/<public_id>
   raw_data BLOB,                        -- if by an external user, then keep the raw json of the follow object
   pending BOOLEAN NOT NULL,             -- whether the follow request is pending

   created TEXT NOT NULL /* Calendar.t */,                -- date at which follow was created
   updated TEXT   /* Calendar.t option */,                -- date at which follow was updated if ever

   author_id INTEGER NOT NULL /* author: int64 */,           -- id of the author of the follow
   target_id INTEGER NOT NULL /* target: int64 */,           -- id of the actor being followed

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
