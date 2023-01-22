  $ ./app_launcher.exe test.db remote-instance create-instance "example.ocamlot.org"
  { Operations.RemoteInstance.id = 1; url = "example.ocamlot.org";
    last_unreachable = None }
  $ ./app_launcher.exe test.db remote-instance create-instance "awesome.xyz"
  { Operations.RemoteInstance.id = 2; url = "awesome.xyz";
    last_unreachable = None }
  $ ./app_launcher.exe test.db remote-instance create-instance "fediverse.caml.uk"
  { Operations.RemoteInstance.id = 3; url = "fediverse.caml.uk";
    last_unreachable = None }
  $ ./app_launcher.exe test.db remote-instance create-instance "random.comp.fedi.sg"
  { Operations.RemoteInstance.id = 4; url = "random.comp.fedi.sg";
    last_unreachable = None }
  $ ./app_launcher.exe test.db remote-instance lookup-instance "example.ocamlot.org"
  { Operations.RemoteInstance.id = 1; url = "example.ocamlot.org";
    last_unreachable = None }
  $ ./app_launcher.exe test.db remote-instance lookup-instance "awesome.xyz"
  { Operations.RemoteInstance.id = 2; url = "awesome.xyz";
    last_unreachable = None }
