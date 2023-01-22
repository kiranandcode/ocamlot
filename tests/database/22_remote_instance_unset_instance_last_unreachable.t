Setup code:
  $ ./app_launcher.exe test.db remote-instance create-instance "example.ocamlot.org" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "awesome.xyz" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "fediverse.caml.uk" > ignore
  $ ./app_launcher.exe test.db remote-instance create-instance "random.comp.fedi.sg" > ignore
Now to party:
  $ ./app_launcher.exe test.db remote-instance update-instance-last-unreachable 2 "2023-01-21T10:40:39-00:00"
  $ ./app_launcher.exe test.db remote-instance lookup-instance "awesome.xyz"
  { Operations.RemoteInstance.id = 2; url = "awesome.xyz";
    last_unreachable = (Some 2023-01-21 10:40:39 +00:00) }
  $ ./app_launcher.exe test.db remote-instance unset-instance-last-unreachable 2
  $ ./app_launcher.exe test.db remote-instance lookup-instance "awesome.xyz"
  { Operations.RemoteInstance.id = 2; url = "awesome.xyz";
    last_unreachable = None }
