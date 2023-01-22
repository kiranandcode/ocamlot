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

Get list of all known remote users

  $ ./app_launcher.exe test.db remote-user get-known-remote-actors 100 0
  remote-bob, fediverse.caml.uk, https://fediverse.caml.uk/remote-bob
  remote-sally, example.ocamlot.org, https://example.ocamlot.org/remote-sally
  remote-joe, random.comp.fedi.sg, https://random.comp.fedi.sg/remote-joe
  remote-bob, random.comp.fedi.sg, https://random.comp.fedi.sg/remote-bob
