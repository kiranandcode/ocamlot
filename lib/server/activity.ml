[@@@warning "-33"]
open Common


let handle_lookup_activity req =
  let uuid = Dream.param req "uuid" in
  let* activity =
    Dream.sql req (Database.Activity.find_by_id ~id:uuid)
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let* activity = return @@ lift_opt ~else_:(fun _ -> `ActivityNotFound uuid)
                              activity in
  Web.activity_json (activity.Database.Activity.raw_data)

let route = 
  Dream.scope "/activity" [] [
    Dream.get "/:uuid" @@ Error_display.handle_error_json handle_lookup_activity
  ]
