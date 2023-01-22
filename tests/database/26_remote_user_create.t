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

  $ ./app_launcher.exe test.db remote-user resolve 1
  { Operations.RemoteUser.id = 1; username = "remote-bob"; instance_id = 3;
    display_name = (Some "Remote bob");
    url = "https://fediverse.caml.uk/remote-bob";
    inbox = (Some "https://fediverse.caml.uk/remote-bob/inbox");
    outbox = (Some "https://fediverse.caml.uk/remote-bob/outbox");
    followers = (Some "https://fediverse.caml.uk/remote-bob/followers");
    following = (Some "https://fediverse.caml.uk/remote-bob/following");
    summary = (Some "I'm a remote user"); public_key_pem = "afasdfasdfasdfad" }
