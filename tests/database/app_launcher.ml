open Containers
open Lwt_result.Syntax

let () =
  Test_utils.run @@ fun db args ->
  let* () = Petrol.VersionedSchema.initialise Database.Tables.db db in
  match[@warning "-8"] args with
  | "echo" :: args ->
    List.iter print_endline args;
    Lwt.return_ok ()
(* * Activity *)
(* ** create *)
  | [ "activity"; "create"; id; data ] ->
(* *** body *)
    let id = (fun x -> x) id in
    let data = Yojson.Safe.from_string data in
    let* _ = Database.Activity.create ~id ~data db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** find by id *)
  | [ "activity"; "find-by-id"; id ] ->
(* *** body *)
    let id = (fun x -> x) id in
    let* res = Database.Activity.find_by_id ~id db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.Activity.show s)
      res;
    Lwt_result.return ()
(* ** update raw data *)
  | [ "activity"; "update-raw-data"; id; raw_data ] ->
(* *** body *)
    let id = (fun x -> x) id in
    let raw_data = Yojson.Safe.from_string raw_data in
    let* _ = Database.Activity.update_raw_data ~id ~raw_data db in
(* pretty print result here? *)
    Lwt_result.return ()
(* * Local user *)
(* ** find user *)
  | [ "local-user"; "find-user"; username ] ->
(* *** body *)
    let username = (fun x -> x) username in
    let* res = Database.LocalUser.find_user ~username db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.LocalUser.show s)
      res;
    Lwt_result.return ()
(* ** resolve *)
  | [ "local-user"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.LocalUser.resolve ~id db in
    (fun s -> print_endline @@ Database.LocalUser.show s) res;
    Lwt_result.return ()
(* ** create user *)
  | [
    "local-user"; "create-user"; display_name; about; manually_accept_follows;
    is_admin; username; password;
  ] ->
(* *** body *)
    let display_name =
      if String.equal display_name "" then None
      else Some ((fun x -> x) display_name)
    in
    let about =
      if String.equal about "" then None else Some ((fun x -> x) about)
    in
    let manually_accept_follows =
      if String.equal manually_accept_follows "" then None
      else
        Some
          ((function
              | "true" -> true
              | "false" -> false
              | s -> failwith ("invalid bool: " ^ s))
             manually_accept_follows)
    in
    let is_admin =
      if String.equal is_admin "" then None
      else
        Some
          ((function
              | "true" -> true
              | "false" -> false
              | s -> failwith ("invalid bool: " ^ s))
             is_admin)
    in
    let username = (fun x -> x) username in
    let password = (fun x -> x) password in
    let* res =
      Database.LocalUser.create_user ?display_name ?about
        ?manually_accept_follows ?is_admin ~username ~password db
    in
    (fun s -> print_endline @@ Database.LocalUser.show s) res;
    Lwt_result.return ()
(* ** login user *)
  | [ "local-user"; "login-user"; username; password ] ->
(* *** body *)
    let username = (fun x -> x) username in
    let password = (fun x -> x) password in
    let* res = Database.LocalUser.login_user ~username ~password db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.LocalUser.show s)
      res;
    Lwt_result.return ()
(* ** update password *)
  | [ "local-user"; "update-password"; id; password ] ->
(* *** body *)
    let id = int_of_string id in
    let password = (fun x -> x) password in
    let* _ = Database.LocalUser.update_password ~id ~password db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** update display name *)
  | [ "local-user"; "update-display-name"; id; display_name ] ->
(* *** body *)
    let id = int_of_string id in
    let display_name = (fun x -> x) display_name in
    let* _ = Database.LocalUser.update_display_name ~id ~display_name db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** update about *)
  | [ "local-user"; "update-about"; id; about ] ->
(* *** body *)
    let id = int_of_string id in
    let about = (fun x -> x) about in
    let* _ = Database.LocalUser.update_about ~id ~about db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** update manually accept follows *)
  | [
    "local-user"; "update-manually-accept-follows"; id; manually_accept_follows;
  ] ->
(* *** body *)
    let id = int_of_string id in
    let manually_accept_follows =
      (function
        | "true" -> true
        | "false" -> false
        | s -> failwith ("invalid bool: " ^ s))
        manually_accept_follows
    in
    let* _ =
      Database.LocalUser.update_manually_accept_follows ~id
        ~manually_accept_follows db
    in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** update is admin *)
  | [ "local-user"; "update-is-admin"; id; is_admin ] ->
(* *** body *)
    let id = int_of_string id in
    let is_admin =
      (function
        | "true" -> true
        | "false" -> false
        | s -> failwith ("invalid bool: " ^ s))
        is_admin
    in
    let* _ = Database.LocalUser.update_is_admin ~id ~is_admin db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** collect local users *)
  | [ "local-user"; "collect-local-users"; offset; limit ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let* res = Database.LocalUser.collect_local_users ?offset ?limit db in
    (fun ls ->
       List.iter (fun s -> print_endline @@ Database.LocalUser.show s) ls)
      res;
    Lwt_result.return ()
(* ** local user count *)
  | [ "local-user"; "local-user-count" ] ->
(* *** body *)
    let* res = Database.LocalUser.local_user_count db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** find local users *)
  | [ "local-user"; "find-local-users"; offset; limit; pattern ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let pattern = (fun x -> x) pattern in
    let* res =
      Database.LocalUser.find_local_users ?offset ?limit ~pattern db
    in
    (fun ls ->
       List.iter (fun s -> print_endline @@ Database.LocalUser.show s) ls)
      res;
    Lwt_result.return ()
(* ** find local user count *)
  | [ "local-user"; "find-local-user-count"; pattern ] ->
(* *** body *)
    let pattern = (fun x -> x) pattern in
    let* res = Database.LocalUser.find_local_user_count ~pattern db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* * Remote instance *)
(* ** lookup instance *)
  | [ "remote-instance"; "lookup-instance"; url ] ->
(* *** body *)
    let url = (fun x -> x) url in
    let* res = Database.RemoteInstance.lookup_instance ~url db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.RemoteInstance.show s)
      res;
    Lwt_result.return ()
(* ** update instance last unreachable *)
  | [
    "remote-instance"; "update-instance-last-unreachable"; id; last_unreachable;
  ] ->
(* *** body *)
    let id = int_of_string id in
    let last_unreachable =
      (fun s ->
         match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
         | Ok (t, _, _) -> t
         | Error (`Msg m) -> failwith m)
        last_unreachable
    in
    let* _ =
      Database.RemoteInstance.update_instance_last_unreachable ~id
        ~last_unreachable db
    in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** unset instance last unreachable *)
  | [ "remote-instance"; "unset-instance-last-unreachable"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* _ = Database.RemoteInstance.unset_instance_last_unreachable ~id db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** resolve *)
  | [ "remote-instance"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.RemoteInstance.resolve ~id db in
    (fun s -> print_endline @@ Database.RemoteInstance.show s) res;
    Lwt_result.return ()
(* ** create instance *)
  | [ "remote-instance"; "create-instance"; url ] ->
(* *** body *)
    let url = (fun x -> x) url in
    let* res = Database.RemoteInstance.create_instance ~url db in
    (fun s -> print_endline @@ Database.RemoteInstance.show s) res;
    Lwt_result.return ()
(* ** find possible remote instances to query *)
  | [
    "remote-instance"; "find-possible-remote-instances-to-query"; offset; limit;
    arg2;
  ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let arg2 = (fun x -> x) arg2 in
    let* res =
      Database.RemoteInstance.find_possible_remote_instances_to_query ?offset
        ?limit arg2 db
    in
    (fun ls ->
       List.iter (fun s -> print_endline @@ Database.RemoteInstance.show s) ls)
      res;
    Lwt_result.return ()
(* * Remote user *)
(* ** lookup remote user by address *)
  | [ "remote-user"; "lookup-remote-user-by-address"; username; url ] ->
(* *** body *)
    let username = (fun x -> x) username in
    let url = (fun x -> x) url in
    let* res =
      Database.RemoteUser.lookup_remote_user_by_address ~username ~url db
    in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.RemoteUser.show s)
      res;
    Lwt_result.return ()
(* ** lookup remote user by url *)
  | [ "remote-user"; "lookup-remote-user-by-url"; url ] ->
(* *** body *)
    let url = (fun x -> x) url in
    let* res = Database.RemoteUser.lookup_remote_user_by_url ~url db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.RemoteUser.show s)
      res;
    Lwt_result.return ()
(* ** resolve *)
  | [ "remote-user"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.RemoteUser.resolve ~id db in
    (fun s -> print_endline @@ Database.RemoteUser.show s) res;
    Lwt_result.return ()
(* ** create remote user *)
  | [
    "remote-user"; "create-remote-user"; display_name; inbox; outbox; followers;
    following; summary; username; instance; url; public_key_pem;
  ] ->
(* *** body *)
    let display_name =
      if String.equal display_name "" then None
      else Some ((fun x -> x) display_name)
    in
    let inbox =
      if String.equal inbox "" then None else Some ((fun x -> x) inbox)
    in
    let outbox =
      if String.equal outbox "" then None else Some ((fun x -> x) outbox)
    in
    let followers =
      if String.equal followers "" then None
      else Some ((fun x -> x) followers)
    in
    let following =
      if String.equal following "" then None
      else Some ((fun x -> x) following)
    in
    let summary =
      if String.equal summary "" then None else Some ((fun x -> x) summary)
    in
    let username = (fun x -> x) username in
    let instance = int_of_string instance in
    let url = (fun x -> x) url in
    let public_key_pem = (fun x -> x) public_key_pem in
    let* res =
      Database.RemoteUser.create_remote_user ?display_name ?inbox ?outbox
        ?followers ?following ?summary ~username ~instance ~url
        ~public_key_pem db
    in
    (fun s -> print_endline @@ Database.RemoteUser.show s) res;
    Lwt_result.return ()
(* ** get known remote actors *)
  | [ "remote-user"; "get-known-remote-actors"; limit; offset ] ->
(* *** body *)
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let* res =
      Database.RemoteUser.get_known_remote_actors ?limit ?offset db
    in
    (fun ls ->
       List.iter (fun (a, b, c) -> print_endline (a ^ ", " ^ b ^ ", " ^ c)) ls)
      res;
    Lwt_result.return ()
(* ** collect remote users *)
  | [ "remote-user"; "collect-remote-users"; offset; limit ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let* res = Database.RemoteUser.collect_remote_users ?offset ?limit db in
    (fun ls ->
       List.iter
         (fun (a, t) -> print_endline (a ^ ": " ^ Database.RemoteUser.show t))
         ls)
      res;
    Lwt_result.return ()
(* ** collect remote users following *)
  | [ "remote-user"; "collect-remote-users-following"; offset; limit; target ]
    ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let target = int_of_string target in
    let* res =
      Database.RemoteUser.collect_remote_users_following ?offset ?limit
        ~target db
    in
    (fun ls ->
       List.iter (fun s -> print_endline @@ Database.RemoteUser.show s) ls)
      res;
    Lwt_result.return ()
(* ** find remote users *)
  | [ "remote-user"; "find-remote-users"; offset; limit; pattern ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let pattern = (fun x -> x) pattern in
    let* res =
      Database.RemoteUser.find_remote_users ?offset ?limit ~pattern db
    in
    (fun ls ->
       List.iter
         (fun (a, t) -> print_endline (a ^ ": " ^ Database.RemoteUser.show t))
         ls)
      res;
    Lwt_result.return ()
(* * Actor *)
(* ** lookup local user *)
  | [ "actor"; "lookup-local-user"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Actor.lookup_local_user ~id db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** lookup remote user *)
  | [ "actor"; "lookup-remote-user"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Actor.lookup_remote_user ~id db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** resolve *)
  | [ "actor"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Actor.resolve ~id db in
    (fun ls ->
       print_endline
         (match ls with
          | `Local i -> "local " ^ string_of_int i
          | `Remote i -> "remote " ^ string_of_int i))
      res;
    Lwt_result.return ()
(* ** create local user *)
  | [ "actor"; "create-local-user"; local_id ] ->
(* *** body *)
    let local_id = int_of_string local_id in
    let* res = Database.Actor.create_local_user ~local_id db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** create remote user *)
  | [ "actor"; "create-remote-user"; remote_id ] ->
(* *** body *)
    let remote_id = int_of_string remote_id in
    let* res = Database.Actor.create_remote_user ~remote_id db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* * Tag *)
(* ** find by name *)
  | [ "tag"; "find-by-name"; tag ] ->
(* *** body *)
    let tag = (fun x -> x) tag in
    let* res = Database.Tag.find_by_name ~tag db in
    (fun s -> print_endline @@ Database.Tag.show s) res;
    Lwt_result.return ()
(* ** resolve *)
  | [ "tag"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Tag.resolve ~id db in
    (fun s -> print_endline @@ Database.Tag.show s) res;
    Lwt_result.return ()
(* ** create *)
  | [ "tag"; "create"; name ] ->
(* *** body *)
    let name = (fun x -> x) name in
    let* res = Database.Tag.create ~name db in
    (fun s -> print_endline @@ Database.Tag.show s) res;
    Lwt_result.return ()
(* * Posts *)
(* ** lookup by public id *)
  | [ "posts"; "lookup-by-public-id"; public_id ] ->
(* *** body *)
    let public_id = (fun x -> x) public_id in
    let* res = Database.Posts.lookup_by_public_id ~public_id db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.Posts.show s)
      res;
    Lwt_result.return ()
(* ** lookup by url *)
  | [ "posts"; "lookup-by-url"; url ] ->
(* *** body *)
    let url = (fun x -> x) url in
    let* res = Database.Posts.lookup_by_url ~url db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.Posts.show s)
      res;
    Lwt_result.return ()
(* ** resolve *)
  | [ "posts"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Posts.resolve ~id db in
    (fun s -> print_endline @@ Database.Posts.show s) res;
    Lwt_result.return ()
(* ** create *)
  | [
    "posts"; "create"; public_id; summary; raw_data; is_public;
    is_follower_public; url; author; post_content; post_source; published;
  ] ->
(* *** body *)
    let public_id =
      if String.equal public_id "" then None
      else Some ((fun x -> x) public_id)
    in
    let summary =
      if String.equal summary "" then None else Some ((fun x -> x) summary)
    in
    let raw_data =
      if String.equal raw_data "" then None
      else Some (Yojson.Safe.from_string raw_data)
    in
    let is_public =
      if String.equal is_public "" then None
      else
        Some
          ((function
              | "true" -> true
              | "false" -> false
              | s -> failwith ("invalid bool: " ^ s))
             is_public)
    in
    let is_follower_public =
      if String.equal is_follower_public "" then None
      else
        Some
          ((function
              | "true" -> true
              | "false" -> false
              | s -> failwith ("invalid bool: " ^ s))
             is_follower_public)
    in
    let url = (fun x -> x) url in
    let author = int_of_string author in
    let post_content =
      (function
        | "markdown" -> `Markdown
        | "org" -> `Org
        | "text" -> `Text
        | s -> failwith ("invalid content-type " ^ s))
        post_content
    in
    let post_source = (fun x -> x) post_source in
    let published =
      (fun s ->
         match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
         | Ok (t, _, _) -> t
         | Error (`Msg m) -> failwith m)
        published
    in
    let* res =
      Database.Posts.create ?public_id ?summary ?raw_data ?is_public
        ?is_follower_public ~url ~author ~post_content ~post_source ~published
        db
    in
    (fun s -> print_endline @@ Database.Posts.show s) res;
    Lwt_result.return ()
(* ** count posts by author *)
  | [ "posts"; "count-posts-by-author"; author ] ->
(* *** body *)
    let author = int_of_string author in
    let* res = Database.Posts.count_posts_by_author ~author db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** collect posts by author *)
  | [ "posts"; "collect-posts-by-author"; offset; limit; start_time; author ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let start_time =
      (fun s ->
         match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
         | Ok (t, _, _) -> t
         | Error (`Msg m) -> failwith m)
        start_time
    in
    let author = int_of_string author in
    let* res =
      Database.Posts.collect_posts_by_author ?offset ?limit ~start_time
        ~author db
    in
    (fun ls -> List.iter (fun s -> print_endline @@ Database.Posts.show s) ls)
      res;
    Lwt_result.return ()
(* ** post to *)
  | [ "posts"; "post-to"; offset; limit; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let id = int_of_string id in
    let* res = Database.Posts.post_to ?offset ?limit ~id db in
    (fun ls -> List.iter (fun i -> print_endline (string_of_int i)) ls) res;
    Lwt_result.return ()
(* ** post cc *)
  | [ "posts"; "post-cc"; offset; limit; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let id = int_of_string id in
    let* res = Database.Posts.post_cc ?offset ?limit ~id db in
    (fun ls -> List.iter (fun i -> print_endline (string_of_int i)) ls) res;
    Lwt_result.return ()
(* ** post mentions *)
  | [ "posts"; "post-mentions"; offset; limit; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let id = int_of_string id in
    let* res = Database.Posts.post_mentions ?offset ?limit ~id db in
    (fun ls -> List.iter (fun i -> print_endline (string_of_int i)) ls) res;
    Lwt_result.return ()
(* ** post tags *)
  | [ "posts"; "post-tags"; offset; limit; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let id = int_of_string id in
    let* res = Database.Posts.post_tags ?offset ?limit ~id db in
    (fun ls -> List.iter (fun i -> print_endline i) ls) res;
    Lwt_result.return ()
(* ** add post to *)
  | [ "posts"; "add-post-to"; id; actor_id ] ->
(* *** body *)
    let id = int_of_string id in
    let actor_id = int_of_string actor_id in
    let* _ = Database.Posts.add_post_to ~id ~actor_id db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** add post tos *)
  | [ "posts"; "add-post-tos"; id; tos ] ->
(* *** body *)
    let id = int_of_string id in
    let tos =
      (fun s -> String.split_on_char ',' s |> List.map int_of_string) tos
    in
    let* _ = Database.Posts.add_post_tos ~id ~tos db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** add post cc *)
  | [ "posts"; "add-post-cc"; id; actor_id ] ->
(* *** body *)
    let id = int_of_string id in
    let actor_id = int_of_string actor_id in
    let* _ = Database.Posts.add_post_cc ~id ~actor_id db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** add post ccs *)
  | [ "posts"; "add-post-ccs"; id; ccs ] ->
(* *** body *)
    let id = int_of_string id in
    let ccs =
      (fun s -> String.split_on_char ',' s |> List.map int_of_string) ccs
    in
    let* _ = Database.Posts.add_post_ccs ~id ~ccs db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** add post mention *)
  | [ "posts"; "add-post-mention"; id; actor_id ] ->
(* *** body *)
    let id = int_of_string id in
    let actor_id = int_of_string actor_id in
    let* _ = Database.Posts.add_post_mention ~id ~actor_id db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** add post mentions *)
  | [ "posts"; "add-post-mentions"; id; mentions ] ->
(* *** body *)
    let id = int_of_string id in
    let mentions =
      (fun s -> String.split_on_char ',' s |> List.map int_of_string) mentions
    in
    let* _ = Database.Posts.add_post_mentions ~id ~mentions db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** add post tag *)
  | [ "posts"; "add-post-tag"; id; tag ] ->
(* *** body *)
    let id = int_of_string id in
    let tag = (fun x -> x) tag in
    let* _ = Database.Posts.add_post_tag ~id ~tag db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** add post tags *)
  | [ "posts"; "add-post-tags"; id; tags ] ->
(* *** body *)
    let id = int_of_string id in
    let tags = (fun s -> String.split_on_char ',' s) tags in
    let* _ = Database.Posts.add_post_tags ~id ~tags db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** collect feed *)
  | [ "posts"; "collect-feed"; offset; limit; start_time; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let start_time =
      if String.equal start_time "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             start_time)
    in
    let id = int_of_string id in
    let* res =
      Database.Posts.collect_feed ?offset ?limit ?start_time ~id db
    in
    (fun ls -> List.iter (fun s -> print_endline @@ Database.Posts.show s) ls)
      res;
    Lwt_result.return ()
(* ** count feed *)
  | [ "posts"; "count-feed"; start_time; id ] ->
(* *** body *)
    let start_time =
      if String.equal start_time "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             start_time)
    in
    let id = int_of_string id in
    let* res = Database.Posts.count_feed ?start_time ~id db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** collect direct *)
  | [ "posts"; "collect-direct"; offset; limit; start_time; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let start_time =
      if String.equal start_time "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             start_time)
    in
    let id = int_of_string id in
    let* res =
      Database.Posts.collect_direct ?offset ?limit ?start_time ~id db
    in
    (fun ls -> List.iter (fun s -> print_endline @@ Database.Posts.show s) ls)
      res;
    Lwt_result.return ()
(* ** count direct *)
  | [ "posts"; "count-direct"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Posts.count_direct ~id db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** collect twkn *)
  | [ "posts"; "collect-twkn"; offset; limit; start_time ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let start_time =
      if String.equal start_time "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             start_time)
    in
    let* res = Database.Posts.collect_twkn ?offset ?limit ?start_time db in
    (fun ls -> List.iter (fun s -> print_endline @@ Database.Posts.show s) ls)
      res;
    Lwt_result.return ()
(* ** count twkn *)
  | [ "posts"; "count-twkn" ] ->
(* *** body *)
    let* res = Database.Posts.count_twkn db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** collect local *)
  | [ "posts"; "collect-local"; offset; limit; start_time ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let start_time =
      if String.equal start_time "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             start_time)
    in
    let* res = Database.Posts.collect_local ?offset ?limit ?start_time db in
    (fun ls -> List.iter (fun s -> print_endline @@ Database.Posts.show s) ls)
      res;
    Lwt_result.return ()
(* ** count local *)
  | [ "posts"; "count-local" ] ->
(* *** body *)
    let* res = Database.Posts.count_local db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* * Follows *)
(* ** lookup by public id *)
  | [ "follows"; "lookup-by-public-id"; public_id ] ->
(* *** body *)
    let public_id = (fun x -> x) public_id in
    let* res = Database.Follows.lookup_by_public_id ~public_id db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.Follows.show s)
      res;
    Lwt_result.return ()
(* ** lookup by url *)
  | [ "follows"; "lookup-by-url"; url ] ->
(* *** body *)
    let url = (fun x -> x) url in
    let* res = Database.Follows.lookup_by_url ~url db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.Follows.show s)
      res;
    Lwt_result.return ()
(* ** resolve *)
  | [ "follows"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Follows.resolve ~id db in
    (fun s -> print_endline @@ Database.Follows.show s) res;
    Lwt_result.return ()
(* ** create *)
  | [
    "follows"; "create"; public_id; raw_data; updated; url; author; target;
    pending; created;
  ] ->
(* *** body *)
    let public_id =
      if String.equal public_id "" then None
      else Some ((fun x -> x) public_id)
    in
    let raw_data =
      if String.equal raw_data "" then None
      else Some (Yojson.Safe.from_string raw_data)
    in
    let updated =
      if String.equal updated "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             updated)
    in
    let url = (fun x -> x) url in
    let author = int_of_string author in
    let target = int_of_string target in
    let pending =
      (function
        | "true" -> true
        | "false" -> false
        | s -> failwith ("invalid bool: " ^ s))
        pending
    in
    let created =
      (fun s ->
         match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
         | Ok (t, _, _) -> t
         | Error (`Msg m) -> failwith m)
        created
    in
    let* res =
      Database.Follows.create ?public_id ?raw_data ?updated ~url ~author
        ~target ~pending ~created db
    in
    (fun s -> print_endline @@ Database.Follows.show s) res;
    Lwt_result.return ()
(* ** update pending status *)
  | [ "follows"; "update-pending-status"; id; pending ] ->
(* *** body *)
    let id = int_of_string id in
    let pending =
      (function
        | "true" -> true
        | "false" -> false
        | s -> failwith ("invalid bool: " ^ s))
        pending
    in
    let* _ = Database.Follows.update_pending_status ~id ~pending db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** delete *)
  | [ "follows"; "delete"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* _ = Database.Follows.delete ~id db in
(* pretty print result here? *)
    Lwt_result.return ()
(* ** collect follows for actor *)
  | [ "follows"; "collect-follows-for-actor"; offset; limit; since; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let since =
      if String.equal since "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             since)
    in
    let id = int_of_string id in
    let* res =
      Database.Follows.collect_follows_for_actor ?offset ?limit ?since ~id db
    in
    (fun s -> print_endline @@ Database.Follows.show s) res;
    Lwt_result.return ()
(* ** is following *)
  | [ "follows"; "is-following"; author; target ] ->
(* *** body *)
    let author = int_of_string author in
    let target = int_of_string target in
    let* res = Database.Follows.is_following ~author ~target db in
    (function
      | true -> print_endline "true"
      | false -> print_endline "false")
      res;
    Lwt_result.return ()
(* ** find follow between *)
  | [ "follows"; "find-follow-between"; author; target ] ->
(* *** body *)
    let author = int_of_string author in
    let target = int_of_string target in
    let* res = Database.Follows.find_follow_between ~author ~target db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.Follows.show s)
      res;
    Lwt_result.return ()
(* ** count following *)
  | [ "follows"; "count-following"; author ] ->
(* *** body *)
    let author = int_of_string author in
    let* res = Database.Follows.count_following ~author db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** collect following for actor *)
  | [ "follows"; "collect-following-for-actor"; offset; limit; since; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let since =
      if String.equal since "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             since)
    in
    let id = int_of_string id in
    let* res =
      Database.Follows.collect_following_for_actor ?offset ?limit ?since ~id
        db
    in
    (fun ls ->
       List.iter (fun s -> print_endline @@ Database.Follows.show s) ls)
      res;
    Lwt_result.return ()
(* ** count followers *)
  | [ "follows"; "count-followers"; target ] ->
(* *** body *)
    let target = int_of_string target in
    let* res = Database.Follows.count_followers ~target db in
    (fun i -> print_endline (string_of_int i)) res;
    Lwt_result.return ()
(* ** collect followers for actor *)
  | [ "follows"; "collect-followers-for-actor"; offset; limit; since; id ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let since =
      if String.equal since "" then None
      else
        Some
          ((fun s ->
              match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
              | Ok (t, _, _) -> t
              | Error (`Msg m) -> failwith m)
             since)
    in
    let id = int_of_string id in
    let* res =
      Database.Follows.collect_followers_for_actor ?offset ?limit ?since ~id
        db
    in
    (fun ls ->
       List.iter (fun s -> print_endline @@ Database.Follows.show s) ls)
      res;
    Lwt_result.return ()
(* * Likes *)
(* ** resolve *)
  | [ "likes"; "resolve"; id ] ->
(* *** body *)
    let id = int_of_string id in
    let* res = Database.Likes.resolve ~id db in
    (fun s -> print_endline @@ Database.Likes.show s) res;
    Lwt_result.return ()
(* ** lookup by url *)
  | [ "likes"; "lookup-by-url"; url ] ->
(* *** body *)
    let url = (fun x -> x) url in
    let* res = Database.Likes.lookup_by_url ~url db in
    (fun s ->
       print_endline
       @@
       match s with
       | None -> "None"
       | Some s -> Database.Likes.show s)
      res;
    Lwt_result.return ()
(* ** create *)
  | [ "likes"; "create"; public_id; raw_data; url; post; actor; published ] ->
(* *** body *)
    let public_id =
      if String.equal public_id "" then None
      else Some ((fun x -> x) public_id)
    in
    let raw_data =
      if String.equal raw_data "" then None
      else Some (Yojson.Safe.from_string raw_data)
    in
    let url = (fun x -> x) url in
    let post = int_of_string post in
    let actor = int_of_string actor in
    let published =
      (fun s ->
         match Ptime.rfc3339_error_to_msg (Ptime.of_rfc3339 s) with
         | Ok (t, _, _) -> t
         | Error (`Msg m) -> failwith m)
        published
    in
    let* res =
      Database.Likes.create ?public_id ?raw_data ~url ~post ~actor ~published
        db
    in
    (fun s -> print_endline @@ Database.Likes.show s) res;
    Lwt_result.return ()
(* ** collect for post *)
  | [ "likes"; "collect-for-post"; offset; limit; post ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let post = int_of_string post in
    let* res = Database.Likes.collect_for_post ?offset ?limit ~post db in
    (fun ls -> List.iter (fun s -> print_endline @@ Database.Likes.show s) ls)
      res;
    Lwt_result.return ()
(* ** collect by actor *)
  | [ "likes"; "collect-by-actor"; offset; limit; actor ] ->
(* *** body *)
    let offset =
      if String.equal offset "" then None else Some (int_of_string offset)
    in
    let limit =
      if String.equal limit "" then None else Some (int_of_string limit)
    in
    let actor = int_of_string actor in
    let* res = Database.Likes.collect_by_actor ?offset ?limit ~actor db in
    (fun ls -> List.iter (fun s -> print_endline @@ Database.Likes.show s) ls)
      res;
    Lwt_result.return ()


