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

It is okay for 2 users on different instances to have the same name:

  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote bob" "https://example.ocamlot.org/remote-bob/inbox" "https://example.ocamlot.org/remote-bob/outbox"  "https://example.ocamlot.org/remote-bob/followers"  "https://example.ocamlot.org/remote-bob/following" "I'm a remote user" "remote-bob" 1 "https://example.ocamlot.org/remote-bob" "afasdfasdfasdfad"
  { Operations.RemoteUser.id = 4; username = "remote-bob"; instance_id = 1;
    display_name = (Some "Remote bob");
    url = "https://example.ocamlot.org/remote-bob";
    inbox = (Some "https://example.ocamlot.org/remote-bob/inbox");
    outbox = (Some "https://example.ocamlot.org/remote-bob/outbox");
    followers = (Some "https://example.ocamlot.org/remote-bob/followers");
    following = (Some "https://example.ocamlot.org/remote-bob/following");
    summary = (Some "I'm a remote user"); public_key_pem = "afasdfasdfasdfad" }

But 2 users on the same instance should not have the same name:

  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote bob" "https://example.ocamlot.org/remote-bob/inbox" "https://example.ocamlot.org/remote-bob/outbox"  "https://example.ocamlot.org/remote-bob/followers"  "https://example.ocamlot.org/remote-bob/following" "I'm a remote user" "remote-bob" 1 "https://example.ocamlot.org/remote-bob" "afasdfasdfasdfad"
  Fatal error: exception Response from <sqlite3:///test.db> failed: UNIQUE constraint failed: remote_user.url. Query: "INSERT INTO remote_user (username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)".
  [2]
