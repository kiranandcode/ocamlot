open Containers
open Common

let log = Logging.add_logger "worker"

let lift_database_error res =
  map_err (fun err -> `DatabaseError (Caqti_error.show err)) res
let lift_resolver_error res =
  map_err (fun err -> `ResolverError err) res

let filter_map_list ~msg f ls =
  Lwt_list.map_s f ls
  |> Lwt.map (List.filter_map (function
    | Ok v -> v
    | Error err ->
      let _, header, details = Error_handling.extract_error_details err in
      log.debug (fun f -> f "%s failed with %s: %s" msg header details);
      None
  ))
  |> lift_pure

let iter_list ~msg f ls =
  Lwt_list.map_s f ls
  |> Lwt.map (List.iter (function
    | Ok _ -> ()
    | Error err ->
      let _, header, details = Error_handling.extract_error_details err in
      log.debug (fun f -> f "%s failed with %s: %s" msg header details);
      ()
  ))
  |> lift_pure



let with_pool pool f = Caqti_lwt.Pool.use f pool

let uri_ends_with_followers to_ =
  Uri.of_string to_ |> Uri.path |> String.split_on_char '/'
  |> List.last_opt |> Option.exists (String.equal "followers")

let extract_local_target_link pool to_ =
  let lazy local_user_regex =
    Configuration.Regex.local_user_id_format in
  if String.equal to_ Activitypub.Constants.ActivityStreams.public
  then return_ok None
  else match Re.exec_opt local_user_regex to_ with
    | Some group ->
      let username = Re.Group.get group 1 in
      let* local_user =
        with_pool pool @@ fun db ->
        Database.LocalUser.find_user ~username db
        |> lift_database_error in
      begin match local_user with
      | None -> return_ok None
      | Some local_user ->
        let* user =
          with_pool pool @@ fun db ->
          Database.Actor.create_local_user ~local_id:(local_user.Database.LocalUser.id) db
          |> lift_database_error in
        return_ok (Some user)
      end
    | None ->
      return_ok None

let handle_error res =
  Lwt.bind res (function
    | Ok _ -> Lwt.return ()
    | Error (#Caqti_error.t as err) ->
      let _, msg, details =
        Error_handling.extract_error_details
          (`DatabaseError (Caqti_error.show err)) in
      log.warning (fun f -> f "worker error: %s" msg);
      log.debug (fun f -> f "worker error details: %s" details);
      Lwt.return ()
    | Error err ->
      let _, msg, details = Error_handling.extract_error_details err in
      log.warning (fun f -> f "worker error: %s" msg);
      log.debug (fun f -> f "worker error details: %s" details);
      Lwt.return ()
  )
