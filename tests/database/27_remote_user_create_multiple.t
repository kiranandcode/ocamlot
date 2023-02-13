Setup instances:
  $ ./app_launcher.exe test.db remote-instance create-instance "example.ocamlot.org" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "awesome.xyz" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "fediverse.caml.uk" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "random.comp.fedi.sg" > ignore

Create a remote user

  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote bob" "https://fediverse.caml.uk/remote-bob/inbox" "https://fediverse.caml.uk/remote-bob/outbox"  "https://fediverse.caml.uk/remote-bob/followers"  "https://fediverse.caml.uk/remote-bob/following" "I'm a remote user" "remote-bob" 3 "https://fediverse.caml.uk/remote-bob" "afasdfasdfasdfad"
  { Operations.RemoteUser.id = 1; username = "remote-bob"; instance_id = 3;
    display_name = (Some "Remote bob");
    url = "https://fediverse.caml.uk/remote-bob";
    inbox = (Some "https://fediverse.caml.uk/remote-bob/inbox");
    outbox = (Some "https://fediverse.caml.uk/remote-bob/outbox");
    followers = (Some "https://fediverse.caml.uk/remote-bob/followers");
    following = (Some "https://fediverse.caml.uk/remote-bob/following");
    summary = (Some "I'm a remote user"); public_key_pem = "afasdfasdfasdfad" }

  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Sally" "https://example.ocamlot.org/remote-bob/inbox" "https://example.ocamlot.org/remote-bob/outbox"  "https://example.ocamlot.org/remote-bob/followers"  "https://example.ocamlot.org/remote-bob/following" "I'm a remote user sally" "remote-sally" 1 "https://example.ocamlot.org/remote-bob" "randompubkeypemgoeshere"
  { Operations.RemoteUser.id = 2; username = "remote-sally"; instance_id = 1;
    display_name = (Some "Remote Sally");
    url = "https://example.ocamlot.org/remote-bob";
    inbox = (Some "https://example.ocamlot.org/remote-bob/inbox");
    outbox = (Some "https://example.ocamlot.org/remote-bob/outbox");
    followers = (Some "https://example.ocamlot.org/remote-bob/followers");
    following = (Some "https://example.ocamlot.org/remote-bob/following");
    summary = (Some "I'm a remote user sally");
    public_key_pem = "randompubkeypemgoeshere" }


  $ ./app_launcher.exe test.db remote-user resolve 1
  { Operations.RemoteUser.id = 1; username = "remote-bob"; instance_id = 3;
    display_name = (Some "Remote bob");
    url = "https://fediverse.caml.uk/remote-bob";
    inbox = (Some "https://fediverse.caml.uk/remote-bob/inbox");
    outbox = (Some "https://fediverse.caml.uk/remote-bob/outbox");
    followers = (Some "https://fediverse.caml.uk/remote-bob/followers");
    following = (Some "https://fediverse.caml.uk/remote-bob/following");
    summary = (Some "I'm a remote user"); public_key_pem = "afasdfasdfasdfad" }
 
