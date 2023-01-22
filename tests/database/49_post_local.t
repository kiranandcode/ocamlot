Setup instances:
  $ ./app_launcher.exe test.db remote-instance create-instance "example.ocamlot.org" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "awesome.xyz" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "fediverse.caml.uk" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "random.comp.fedi.sg" > ignore

Setup local users:
  $ ./app_launcher.exe test.db local-user create-user "" "" false false joe shmoe > ignore
  $ ./app_launcher.exe test.db local-user create-user "" "" false false sally wally > ignore
  $ ./app_launcher.exe test.db local-user create-user "hello" "" false true barry harry > ignore

Setup remote users:
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote bob" "https://fediverse.caml.uk/remote-bob/inbox" "https://fediverse.caml.uk/remote-bob/outbox"  "https://fediverse.caml.uk/remote-bob/followers"  "https://fediverse.caml.uk/remote-bob/following" "I'm a remote user" "remote-bob" 3 "https://fediverse.caml.uk/remote-bob" "afasdfasdfasdfad" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Sally" "https://example.ocamlot.org/remote-sally/inbox" "https://example.ocamlot.org/remote-sally/outbox"  "https://example.ocamlot.org/remote-sally/followers"  "https://example.ocamlot.org/remote-sally/following" "I'm a remote user sally" "remote-sally" 1 "https://example.ocamlot.org/remote-sally" "randompubkeypemgoeshere" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Joe" "https://random.comp.fedi.sg/remote-joe/inbox" "https://random.comp.fedi.sg/remote-joe/outbox"  "https://random.comp.fedi.sg/remote-joe/followers"  "https://random.comp.fedi.sg/remote-joe/following" "I'm a remote user joe" "remote-joe" 4 "https://random.comp.fedi.sg/remote-joe" "randompubkeypemgoeshere" > ignore


Setup actors:

 - Actor 1 <- remote user 1:
  $ ./app_launcher.exe test.db actor create-remote-user 1 > ignore
 - Actor 2 <- remote user 2:
  $ ./app_launcher.exe test.db actor create-remote-user 2 > ignore
 - Actor 3 <- remote user 3:
  $ ./app_launcher.exe test.db actor create-remote-user 3 > ignore
 - Actor 4 <- local user 1:
  $ ./app_launcher.exe test.db actor create-local-user 1 > ignore
 - Actor 5 <- remote user 4:
  $ ./app_launcher.exe test.db actor create-remote-user 4 > ignore
 - Actor 6 <- local user 2:
  $ ./app_launcher.exe test.db actor create-local-user 2 > ignore
 - Actor 6 <- local user 3:
  $ ./app_launcher.exe test.db actor create-local-user 3 > ignore
 - Actor 6 <- local user 4:
  $ ./app_launcher.exe test.db actor create-local-user 4 > ignore

Posts:

  $ ./app_launcher.exe test.db posts create "pub-id-1" "" "" true true "https://localhost/joe/posts/pub-id-1" 4 "text" "First post" "2023-01-01T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "pub-id-2" "" "" false true "https://localhost/joe/posts/pub-id-2" 4 "markdown" "Second post" "2023-01-02T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "pub-id-3" "" "" true false "https://localhost/joe/posts/pub-id-3" 4 "org" "Third post" "2023-01-03T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "pub-id-4" "" "" false false "https://localhost/joe/posts/pub-id-4" 4 "text" "Fourth post" "2023-01-04T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "pub-id-5" "" "" true true "https://localhost/joe/posts/pub-id-4" 4 "text" "Fifth post" "2023-01-05T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "pub-id-4" "" "" true true "https://localhost/joe/posts/pub-id-5" 4 "text" "Fifth post" "2023-01-05T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A summary" "{\"id\": \"remote-id-1\", \"content\": \"...\"}" true true "https://fediverse.caml.uk/remote-bob/posts/remote-id-1" 1 "text" "First remote post" "2023-01-06T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A summary" "{\"id\": \"remote-id-4\", \"content\": \"...\"}" true false "https://fediverse.caml.uk/remote-bob/posts/remote-id-4" 1 "text" "Second remote post" "2023-01-09T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A second summary" "{\"id\": \"remote-id-5\", \"content\": \"...\"}" false true "https://fediverse.caml.uk/remote-bob/posts/remote-id-5" 1 "text" "Third remote post" "2023-01-10T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A third summary" "{\"id\": \"remote-id-6\", \"content\": \"...\"}" false true "https://fediverse.caml.uk/remote-bob/posts/remote-id-6" 1 "text" "Fourth remote post" "2023-01-11T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A fourth summary" "{\"id\": \"remote-id-7\", \"content\": \"...\"}" false true "https://fediverse.caml.uk/remote-bob/posts/remote-id-7" 1 "text" "Fifth remote post" "2023-01-12T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A second post" "{\"id\": \"remote-id-2\", \"content\": \"...\"}" true true "https://example.ocamlot.org/remote-sally/posts/remote-id-2" 2 "text" "Second remote post" "2023-01-07T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A third post" "{\"id\": \"remote-id-3\", \"content\": \"...\"}" true true "https://example.ocamlot.org/remote-sally/posts/remote-id-3" 2 "text" "Third remote post" "2023-01-08T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A fourth post" "{\"id\": \"remote-id-4\", \"content\": \"...\"}" true true "https://example.ocamlot.org/remote-sally/posts/remote-id-4" 2 "text" "Fourth remote post" "2023-01-09T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A fifth post" "{\"id\": \"remote-id-5\", \"content\": \"...\"}" true true "https://example.ocamlot.org/remote-sally/posts/remote-id-5" 2 "text" "Fifth remote post" "2023-01-10T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A third post" "{\"id\": \"remote-id-3\", \"content\": \"...\"}" true true "https://random.comp.fedi.sg/remote-joe/posts/remote-id-3" 3 "text" "Third remote post" "2023-01-08T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A fourth post" "{\"id\": \"remote-id-4\", \"content\": \"...\"}" true true "https://random.comp.fedi.sg/remote-joe/posts/remote-id-4" 3 "text" "Fourth remote post" "2023-01-09T10:40:39-00:00" > ignore
  $ ./app_launcher.exe test.db posts create "" "A fifth post" "{\"id\": \"remote-id-5\", \"content\": \"...\"}" true true "https://random.comp.fedi.sg/remote-joe/posts/remote-id-5" 3 "text" "Fifth remote post" "2023-01-10T10:40:39-00:00" > ignore

Tests:

  $ ./app_launcher.exe test.db posts add-post-to 1 1

  $ ./app_launcher.exe test.db posts add-post-cc 1 2

  $ ./app_launcher.exe test.db posts add-post-mention 1 3

  $ ./app_launcher.exe test.db posts add-post-mention 1 1

  $ ./app_launcher.exe test.db posts add-post-to 2 1

  $ ./app_launcher.exe test.db posts add-post-cc 2 4

  $ ./app_launcher.exe test.db posts count-local
  3

  $ ./app_launcher.exe test.db posts collect-local 0 100 "2023-01-29T10:40:39-00:00"
  { Operations.Posts.id = 5; public_id = (Some "pub-id-4");
    url = "https://localhost/joe/posts/pub-id-5"; author_id = 4;
    is_public = true; is_follower_public = true; summary = None;
    content_type = `Text; post_source = "Fifth post";
    published = 2023-01-05 10:40:39 +00:00; raw_data = None }
  { Operations.Posts.id = 3; public_id = (Some "pub-id-3");
    url = "https://localhost/joe/posts/pub-id-3"; author_id = 4;
    is_public = true; is_follower_public = false; summary = None;
    content_type = `Org; post_source = "Third post";
    published = 2023-01-03 10:40:39 +00:00; raw_data = None }
  { Operations.Posts.id = 1; public_id = (Some "pub-id-1");
    url = "https://localhost/joe/posts/pub-id-1"; author_id = 4;
    is_public = true; is_follower_public = true; summary = None;
    content_type = `Text; post_source = "First post";
    published = 2023-01-01 10:40:39 +00:00; raw_data = None }
