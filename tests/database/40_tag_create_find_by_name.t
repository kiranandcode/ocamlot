  $ ./app_launcher.exe test.db tag create "2022"
  { Operations.Tag.id = 1; name = "2022" }

  $ ./app_launcher.exe test.db tag create "2023"
  { Operations.Tag.id = 2; name = "2023" }

  $ ./app_launcher.exe test.db tag create "fun"
  { Operations.Tag.id = 3; name = "fun" }

  $ ./app_launcher.exe test.db tag create "ocaml"  
  { Operations.Tag.id = 4; name = "ocaml" }

Find by name is an internal function that assumes the tag being searched for exists:

  $ ./app_launcher.exe test.db tag find-by-name fun
  { Operations.Tag.id = 3; name = "fun" }

  $ ./app_launcher.exe test.db tag find-by-name holidays
  Fatal error: exception Unexpected result from <sqlite3:///test.db>: Received no rows for find. Query: "SELECT tags.tag_id, tags.tag_name\nFROM tags\nWHERE tags.tag_name = ?1".
  [2]
