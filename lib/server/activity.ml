[@@@warning "-33"]
open Common


let handle_lookup_activity _config req =
  let uuid = Dream.param req "uuid" in
  let+ activity =
    Dream.sql req (Database.Activity.find_by_id ~id:uuid)
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let+ activity = return @@ lift_opt ~else_:(fun _ -> `ActivityNotFound uuid)
                              activity in
  activity_json (activity.Database.Activity.raw_data)

let route config = 
  Dream.scope "/activity" [] [
    Dream.get "/:uuid" @@ Error_handling.handle_error_json config (handle_lookup_activity config)
  ]
