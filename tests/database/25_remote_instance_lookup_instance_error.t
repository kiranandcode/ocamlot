Setup code:
  $ ./app_launcher.exe test.db remote-instance create-instance "example.ocamlot.org" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "awesome.xyz" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "fediverse.caml.uk" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "random.comp.fedi.sg" > ignore

Looking up existing instances:

  $ ./app_launcher.exe test.db remote-instance lookup-instance "random.comp.fedi.sg"
  { Operations.RemoteInstance.id = 4; url = "random.comp.fedi.sg";
    last_unreachable = None }

Looking up non-existant instances:

  $ ./app_launcher.exe test.db remote-instance lookup-instance "does-not-exist.org"
  None
