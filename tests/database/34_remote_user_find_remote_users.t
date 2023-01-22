Setup instances:
  $ ./app_launcher.exe test.db remote-instance create-instance "example.ocamlot.org" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "awesome.xyz" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "fediverse.caml.uk" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "random.comp.fedi.sg" > ignore

Setup remote users:
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote bob" "https://fediverse.caml.uk/remote-bob/inbox" "https://fediverse.caml.uk/remote-bob/outbox"  "https://fediverse.caml.uk/remote-bob/followers"  "https://fediverse.caml.uk/remote-bob/following" "I'm a remote user" "remote-bob" 3 "https://fediverse.caml.uk/remote-bob" "afasdfasdfasdfad" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Sally" "https://example.ocamlot.org/remote-sally/inbox" "https://example.ocamlot.org/remote-sally/outbox"  "https://example.ocamlot.org/remote-sally/followers"  "https://example.ocamlot.org/remote-sally/following" "I'm a remote user sally" "remote-sally" 1 "https://example.ocamlot.org/remote-sally" "randompubkeypemgoeshere" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Joe" "https://random.comp.fedi.sg/remote-joe/inbox" "https://random.comp.fedi.sg/remote-joe/outbox"  "https://random.comp.fedi.sg/remote-joe/followers"  "https://random.comp.fedi.sg/remote-joe/following" "I'm a remote user joe" "remote-joe" 4 "https://random.comp.fedi.sg/remote-joe" "randompubkeypemgoeshere" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Bob" "https://random.comp.fedi.sg/remote-bob/inbox" "https://random.comp.fedi.sg/remote-bob/outbox"  "https://random.comp.fedi.sg/remote-bob/followers"  "https://random.comp.fedi.sg/remote-bob/following" "I'm a remote user bob" "remote-bob" 4 "https://random.comp.fedi.sg/remote-bob" "randompubkeypemgoeshere" > ignore

Get list of remote users matching a pattern

  $ ./app_launcher.exe test.db remote-user find-remote-users 0 100 "%a%"
  example.ocamlot.org: { Operations.RemoteUser.id = 2; username = "remote-sally"; instance_id = 1;
    display_name = (Some "Remote Sally");
    url = "https://example.ocamlot.org/remote-sally";
    inbox = (Some "https://example.ocamlot.org/remote-sally/inbox");
    outbox = (Some "https://example.ocamlot.org/remote-sally/outbox");
    followers = (Some "https://example.ocamlot.org/remote-sally/followers");
    following = (Some "https://example.ocamlot.org/remote-sally/following");
    summary = (Some "I'm a remote user sally");
    public_key_pem = "randompubkeypemgoeshere" }
  $ ./app_launcher.exe test.db remote-user find-remote-users 0 100 "%o%"
  example.ocamlot.org: { Operations.RemoteUser.id = 2; username = "remote-sally"; instance_id = 1;
    display_name = (Some "Remote Sally");
    url = "https://example.ocamlot.org/remote-sally";
    inbox = (Some "https://example.ocamlot.org/remote-sally/inbox");
    outbox = (Some "https://example.ocamlot.org/remote-sally/outbox");
    followers = (Some "https://example.ocamlot.org/remote-sally/followers");
    following = (Some "https://example.ocamlot.org/remote-sally/following");
    summary = (Some "I'm a remote user sally");
    public_key_pem = "randompubkeypemgoeshere" }
  random.comp.fedi.sg: { Operations.RemoteUser.id = 3; username = "remote-joe"; instance_id = 4;
    display_name = (Some "Remote Joe");
    url = "https://random.comp.fedi.sg/remote-joe";
    inbox = (Some "https://random.comp.fedi.sg/remote-joe/inbox");
    outbox = (Some "https://random.comp.fedi.sg/remote-joe/outbox");
    followers = (Some "https://random.comp.fedi.sg/remote-joe/followers");
    following = (Some "https://random.comp.fedi.sg/remote-joe/following");
    summary = (Some "I'm a remote user joe");
    public_key_pem = "randompubkeypemgoeshere" }
  fediverse.caml.uk: { Operations.RemoteUser.id = 1; username = "remote-bob"; instance_id = 3;
    display_name = (Some "Remote bob");
    url = "https://fediverse.caml.uk/remote-bob";
    inbox = (Some "https://fediverse.caml.uk/remote-bob/inbox");
    outbox = (Some "https://fediverse.caml.uk/remote-bob/outbox");
    followers = (Some "https://fediverse.caml.uk/remote-bob/followers");
    following = (Some "https://fediverse.caml.uk/remote-bob/following");
    summary = (Some "I'm a remote user"); public_key_pem = "afasdfasdfasdfad" }
  random.comp.fedi.sg: { Operations.RemoteUser.id = 4; username = "remote-bob"; instance_id = 4;
    display_name = (Some "Remote Bob");
    url = "https://random.comp.fedi.sg/remote-bob";
    inbox = (Some "https://random.comp.fedi.sg/remote-bob/inbox");
    outbox = (Some "https://random.comp.fedi.sg/remote-bob/outbox");
    followers = (Some "https://random.comp.fedi.sg/remote-bob/followers");
    following = (Some "https://random.comp.fedi.sg/remote-bob/following");
    summary = (Some "I'm a remote user bob");
    public_key_pem = "randompubkeypemgoeshere" }
