 INSERT OR IGNORE INTO Activity (id, raw_data)  VALUES (?, ?)

;;

 SELECT id, raw_data FROM Activity WHERE id = ?

;;

 UPDATE OR IGNORE Activity SET raw_data = ? WHERE id = ?

;;

 SELECT id FROM Actor WHERE local_id = ?

;;

 SELECT id FROM Actor WHERE remote_id = ?

;;

 SELECT local_id, remote_id FROM Actor WHERE id = ?

;;

 INSERT OR IGNORE INTO Actor (local_id)  VALUES (?)

;;

 INSERT OR IGNORE INTO Actor (remote_id)  VALUES (?)

;;

INSERT OR IGNORE
INTO Follows (public_id, url, raw_data, pending, created, updated, author_id, target_id)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE public_id = ?

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE url = ?

;;

 UPDATE Follows SET pending = ?, updated = ? WHERE id = ?

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE id = ?

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND DATETIME(COALESCE(updated, created)) <= ? AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?

;;

SELECT COUNT(*)
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?

;;

SELECT COUNT(*)
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC

;;

SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?

;;

DELETE FROM Follows
WHERE id = ?
LIMIT 1

;;

SELECT id, public_id, url, raw_data, published, post_id, actor_id FROM Likes WHERE id = ?

;;

INSERT OR IGNORE
INTO Likes (public_id, url, raw_data, published, post_id, actor_id)
VALUES (?, ?, ?, ?, ?, ?)

;;

SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE post_id = ?
ORDER BY datetime(published)

;;

SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE actor_id = ?
ORDER BY datetime(published)

;;

SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE actor_id = ? AND published <= ?
ORDER BY datetime(published) DESC
LIMIT ? OFFSET ?

;;

SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE url = ?

;;

SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE public_id = ?

;;

INSERT INTO LocalUser (username, password, about, manually_accept_follows, is_admin, pubkey, privkey)
 VALUES (?, ?, ?, ?, ?, ?, ?)

;;

SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE username = ?

;;

SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE id = ?

;;

UPDATE LocalUser
SET about = ?
WHERE id = ?

;;

UPDATE LocalUser
SET is_admin = ?
WHERE id = ?

;;

UPDATE LocalUser
SET manually_accept_follows = ?
WHERE id = ?

;;

INSERT OR IGNORE
INTO Posts (public_id, url, author_id, is_public, summary, post_source, published, raw_data)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)

;;

SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE public_id = ?

;;

SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE url = ?

;;

SELECT COUNT(*)
FROM Posts
WHERE author_id = ?

;;

SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE author_id = ? AND is_public = TRUE
ORDER BY DATETIME(published) DESC

;;

SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE author_id = ? AND DATETIME(published) <= ? AND is_public = TRUE
ORDER BY DATETIME(published) DESC
LIMIT ? OFFSET ?

;;

SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE id = ?

;;

INSERT OR IGNORE
INTO PostTo (post_id, actor_id)
VALUES (?, ?)

;;

SELECT actor_id
From PostTo
WHERE post_id = ?

;;

INSERT OR IGNORE
INTO PostCc (post_id, actor_id)
VALUES (?, ?)

;;

SELECT actor_id
From PostCc
WHERE post_id = ?

;;

INSERT OR IGNORE
INTO PostTags (post_id, tag_id, url)
VALUES (?,?,?)

;;

SELECT Tags.tag_id, Tags.tag_name, PostTags.url
FROM PostTags
JOIN Tags ON PostTags.tag_id = Tags.tag_id
WHERE PostTags.post_id = ?

;;

INSERT OR IGNORE
INTO Mention (post_id, actor_id)
VALUES (?,?)

;;

SELECT actor_id
FROM Mention
WHERE post_id = ?

;;

-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND (
    -- where, we (1) are the author
    P.author_id = ? OR
-- or	we (1) are following the author of the post, and the post is public
    (EXISTS (SELECT * FROM Follows AS F WHERE F.author_id = ? AND F.target_id = P.author_id) AND P.is_public = TRUE) OR
-- or we (1) are the recipients (cc, to) of the post    
    (EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)))
ORDER BY DATETIME(P.published) DESC

;;

-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND 
    DATETIME(P.published) <= ? AND (
    -- where, we (1) are the author
    P.author_id = ? OR
-- or	we (1) are following the author of the post, and the post is public
    (EXISTS (SELECT * FROM Follows AS F WHERE F.author_id = ? AND F.target_id = P.author_id) AND P.is_public = TRUE) OR
-- or we (1) are the recipients (cc, to) of the post    
    (EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)))
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?

;;

-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND (
    -- where, we (1) are the author and the post is public
    (P.author_id = ? AND P.is_public = FALSE) OR
    -- or we (1) are the recipients (cc, to) of the post    
    ((EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)) AND
      P.is_public = FALSE))
ORDER BY DATETIME(P.published) DESC

;;

-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
    DATETIME(P.published) <= ? AND (
    -- where, we (1) are the author and the post is public
    (P.author_id = ? AND P.is_public = FALSE) OR
    -- or we (1) are the recipients (cc, to) of the post    
    ((EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)) AND
      P.is_public = FALSE))
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?

;;

-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND (P.is_public = TRUE)
ORDER BY DATETIME(P.published) DESC

;;

-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
    DATETIME(P.published) <= ? AND
    (P.is_public = TRUE)
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?

;;

SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
    (P.is_public = TRUE) AND 
EXISTS (SELECT * FROM Actor as Act WHERE Act.id = P.author_id AND Act.local_id IS NOT NULL) 
ORDER BY DATETIME(P.published) DESC

;;

SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
   DATETIME(P.published) <= ? AND
    (P.is_public = TRUE) AND 
EXISTS (SELECT * FROM Actor as Act WHERE Act.id = P.author_id AND Act.local_id IS NOT NULL) 
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?

;;

INSERT OR IGNORE INTO RemoteInstance (url, last_unreachable)  VALUES (?, ?)

;;

SELECT id, url, last_unreachable FROM RemoteInstance WHERE id = ?

;;

SELECT id, url, last_unreachable FROM RemoteInstance WHERE url = ?

;;

UPDATE RemoteInstance SET last_unreachable = ? WHERE id = ?

;;

INSERT INTO RemoteUser (username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem)  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)

;;

SELECT
    RemoteUser.id,
    RemoteUser.username,
    RemoteUser.instance_id,
    RemoteUser.display_name,
    RemoteUser.url,
    RemoteUser.inbox,
    RemoteUser.outbox,
    RemoteUser.followers,
    RemoteUser.following,
    RemoteUser.summary,
    RemoteUser.public_key_pem
FROM RemoteUser JOIN RemoteInstance on RemoteUser.instance_id = RemoteInstance.id 
WHERE RemoteInstance.url = ? AND RemoteUser.username = ?

;;

 SELECT id, username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem FROM RemoteUser WHERE url = ?

;;

 SELECT id, username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem FROM RemoteUser WHERE id = ?

;;

SELECT RemoteUser.username, RemoteInstance.url, RemoteUser.url FROM RemoteUser 
JOIN RemoteInstance on RemoteUser.instance_id = RemoteInstance.id

;;

-- select users from remote users
SELECT
    RU.id,
    RU.username,
    RU.instance_id,
    RU.display_name,
    RU.url,
    RU.inbox,
    RU.outbox,
    RU.followers,
    RU.following,
    RU.summary,
    RU.public_key_pem
FROM RemoteUser as RU
JOIN Actor as A ON A.remote_id = RU.id
WHERE
  -- where there exists a follow
  EXISTS (
      SELECT *
     FROM Follows AS F
     -- where the author is the remote user
     WHERE F.author_id = A.id
       -- the target is our guy
       AND F.target_id = ?
       -- and the follow is not pending
       AND F.pending = FALSE)
ORDER BY RU.id ASC

;;

-- select users from remote users
SELECT
    RU.id,
    RU.username,
    RU.instance_id,
    RU.display_name,
    RU.url,
    RU.inbox,
    RU.outbox,
    RU.followers,
    RU.following,
    RU.summary,
    RU.public_key_pem
FROM RemoteUser as RU
JOIN Actor as A ON A.remote_id = RU.id
WHERE
  -- where there exists a follow
  EXISTS (
      SELECT *
     FROM Follows AS F
     -- where the author is the remote user
     WHERE F.author_id = A.id
       -- the target is our guy
       AND F.target_id = ?
       -- and the follow is not pending
       AND F.pending = FALSE)
ORDER BY RU.id ASC
LIMIT ? OFFSET ?

;;

INSERT OR IGNORE INTO Tags (tag_name) VALUES (?)

;;

SELECT tag_id, tag_name
FROM Tags
WHERE tag_name = ?

;;

SELECT tag_id, tag_name
FROM Tags
WHERE tag_id = ?
