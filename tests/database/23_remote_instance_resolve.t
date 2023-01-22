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
  $ ./app_launcher.exe test.db remote-instance resolve 2
  { Operations.RemoteInstance.id = 2; url = "awesome.xyz";
    last_unreachable = (Some 2023-01-21 10:40:39 +00:00) }
  $ ./app_launcher.exe test.db remote-instance resolve 3
  { Operations.RemoteInstance.id = 3; url = "fediverse.caml.uk";
    last_unreachable = None }


Resolve is an internal API that assumes that the internal constraints of the database are satisfied, so it doesn't account for the case in which it is called with an invalid link.

  $ ./app_launcher.exe test.db remote-instance resolve 100
  Fatal error: exception Unexpected result from <sqlite3:///test.db>: Received no rows for find. Query: "SELECT remote_instance.id, remote_instance.url, remote_instance.last_unreachable\nFROM remote_instance\nWHERE remote_instance.id = ?1".
  [2]
