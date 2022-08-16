[@@@warning "-33"]
open Common

(* let with_activity req then_ =
 *   let load_activity req uuid =
 *     match Database.Activity.id_from_string uuid with
 *     | None -> Lwt.return_error "invalid uuid"
 *     | Some uuid -> Dream.sql req (Database.Activity.find uuid) in
 *   with_param "uuid" load_activity req ~then_ ~else_:(not_found ~msg:"Activity not found")
 * 
 * let handle_lookup_activity _config req =
 *   let> activity = with_activity req in
 *   activity_json (Database.Activity.data activity)
 * 
 * let route config = 
 *   Dream.scope "/activity" [] [
 *     Dream.get "/:uuid" (handle_lookup_activity config)
 *   ] *)
