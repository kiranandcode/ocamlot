Setup code:
  $ ./app_launcher.exe test.db remote-instance create-instance "example.ocamlot.org" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "awesome.xyz" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "fediverse.caml.uk" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "random.comp.fedi.sg" > ignore

Setup remote users:
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote bob" "https://fediverse.caml.uk/remote-bob/inbox" "https://fediverse.caml.uk/remote-bob/outbox"  "https://fediverse.caml.uk/remote-bob/followers"  "https://fediverse.caml.uk/remote-bob/following" "I'm a remote user" "remote-bob" 3 "https://fediverse.caml.uk/remote-bob" "afasdfasdfasdfad" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Sally" "https://example.ocamlot.org/remote-sally/inbox" "https://example.ocamlot.org/remote-sally/outbox"  "https://example.ocamlot.org/remote-sally/followers"  "https://example.ocamlot.org/remote-sally/following" "I'm a remote user sally" "remote-sally" 1 "https://example.ocamlot.org/remote-sally" "randompubkeypemgoeshere" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Joe" "https://random.comp.fedi.sg/remote-joe/inbox" "https://random.comp.fedi.sg/remote-joe/outbox"  "https://random.comp.fedi.sg/remote-joe/followers"  "https://random.comp.fedi.sg/remote-joe/following" "I'm a remote user joe" "remote-joe" 4 "https://random.comp.fedi.sg/remote-joe" "randompubkeypemgoeshere" > ignore
  $ ./app_launcher.exe test.db remote-user create-remote-user "Remote Bob" "https://random.comp.fedi.sg/remote-bob/inbox" "https://random.comp.fedi.sg/remote-bob/outbox"  "https://random.comp.fedi.sg/remote-bob/followers"  "https://random.comp.fedi.sg/remote-bob/following" "I'm a remote user bob" "remote-bob" 4 "https://random.comp.fedi.sg/remote-bob" "randompubkeypemgoeshere" > ignore


Find possible remote instances to query:
  $ ./app_launcher.exe test.db remote-instance find-possible-remote-instances-to-query 0 100 "%bob%"
  { Operations.RemoteInstance.id = 4; url = "random.comp.fedi.sg";
    last_unreachable = None }
  { Operations.RemoteInstance.id = 3; url = "fediverse.caml.uk";
    last_unreachable = None }
