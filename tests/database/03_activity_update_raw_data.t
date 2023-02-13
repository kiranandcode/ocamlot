  $ ./app_launcher.exe test.db activity create id-0 "{ \"bob\": \"epic\" }"
  $ ./app_launcher.exe test.db activity create id-1 "{ \"sally\": 1, \"harry\": [{\"age\": 18, \"barry\": false }] }"
  $ ./app_launcher.exe test.db activity find-by-id id-1
  { Operations.Activity.id = "id-1";
    raw_data =
    `Assoc ([("sally", `Int (1));
              ("harry",
               `List ([`Assoc ([("age", `Int (18)); ("barry", `Bool (false))])]))
              ])
    }
  $ ./app_launcher.exe test.db activity update-raw-data id-0 "{ \"bob\": [\"epic\"], \"alice\": true }"
  $ ./app_launcher.exe test.db activity find-by-id id-0
  { Operations.Activity.id = "id-0";
    raw_data =
    `Assoc ([("bob", `List ([`String ("epic")])); ("alice", `Bool (true))]) }
