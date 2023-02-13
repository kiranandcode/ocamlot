  $ ./app_launcher.exe test.db activity create id-0 "{ \"bob\": \"epic\" }"
  $ ./app_launcher.exe test.db activity find-by-id id-0
  { Operations.Activity.id = "id-0";
    raw_data = `Assoc ([("bob", `String ("epic"))]) }
