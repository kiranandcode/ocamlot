[@@@warning "-33"]
open Common

let handle_lookup_activity _config req =
  let param = Dream.param req "uuid" in
  let+ uuid = return @@ lift_opt ~else_:(fun _ -> `InvalidData "invalid uuid")
                          (Database.Activity.id_from_string param) in
  let+ activity =
    Dream.sql req (Database.Activity.find uuid)
    |> map_err (fun err -> `DatabaseError err) in
  let+ activity = return @@ lift_opt ~else_:(fun _ -> `ActivityNotFound param)
                              activity in
  activity_json (Database.Activity.data activity)

let route config = 
  Dream.scope "/activity" [] [
    Dream.get "/:uuid" @@ Error_handling.handle_error_json config (handle_lookup_activity config)
  ]
