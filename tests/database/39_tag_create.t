  $ ./app_launcher.exe test.db tag create "2022"
  { Operations.Tag.id = 1; name = "2022" }

  $ ./app_launcher.exe test.db tag create "2023"
  { Operations.Tag.id = 2; name = "2023" }

  $ ./app_launcher.exe test.db tag create "fun"
  { Operations.Tag.id = 3; name = "fun" }

  $ ./app_launcher.exe test.db tag create "ocaml"  
  { Operations.Tag.id = 4; name = "ocaml" }

  $ ./app_launcher.exe test.db tag create "2022"  
  { Operations.Tag.id = 1; name = "2022" }

  $ ./app_launcher.exe test.db tag resolve 2
  { Operations.Tag.id = 2; name = "2023" }

  $ ./app_launcher.exe test.db tag resolve 4
  { Operations.Tag.id = 4; name = "ocaml" }
