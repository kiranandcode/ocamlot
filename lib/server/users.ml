[@@@warning "-33"]
open Containers
open Common

module StringSet = Set.Make(String)

let log = Logging.add_logger "web.user"
module H = Tyxml.Html
(* * Utils  *)
let sql req op =
  Dream.sql req op
  |> map_err (fun err -> `DatabaseError (Caqti_error.show err))

let check_input_size field max_size data =
  if String.length data > max_size
  then Error (
    `InputTooLarge (
      field,
      max_size,
      "[" ^
      string_of_int (String.length data) ^
      "]" ^
      String.take 100 data ^
      "..."
    ))
  else Ok data

let lift_pair_fst f (l,r) =
  match f l with
  | Ok l -> Ok (l,r)
  | Error _ as err -> err
let lift_pair_snd f (l,r) =
  match f r with
  | Ok r -> Ok (l,r)
  | Error _ as err -> err


let lift_result_check f = function
    None -> Ok None
  | Some data -> f data |> Result.map Option.some

let get_user_checked req username =
  let* current_user = current_user req in
  match current_user with
  | None ->
    return_ok None
  | Some current_user when not (String.equal username current_user.Database.LocalUser.username) ->
    return (Error (`Unauthorised ("Don't have permission to edit " ^ username ^ "'s profile")))
  | Some current_user ->
    return_ok (Some current_user)

let extract_single_multipart_data = function
  | [(None, data)] -> Ok data
  | _ -> Error (`InvalidData "expected a single text input")

let extract_file_multipart_data = function
  | [(Some fname, data)] -> Ok (fname, data)
  | _ -> Error (`InvalidData "expected a file input")

let extract_user req (user: Database.LocalUser.t) =
  let* no_following, no_followers, _no_posts =
    Dream.sql req (fun db ->
      let* user = Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id) db in
      let* following = Database.Follows.count_following ~author:user db in
      let* followers = Database.Follows.count_followers ~target:user db in
      let* posts = Database.Posts.count_posts_by_author ~author:user db in
      Lwt.return_ok (following, followers,posts)
    ) |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let+ current_user = current_user req in
  let actions = match current_user with
    | Some current_user when String.equal current_user.username user.username ->
      View.Utils.[true, {url="/users/" ^ user.username ^ "/edit"; text="Edit"; form=None}]
    | _ ->
      View.Utils.[true, {url="/users/" ^ user.username ^ "/follow"; text="Follow"; form=None};
                  false, {url="/users/" ^ user.username ^ "/mute"; text="Mute"; form=None};
                  false, {url="/users/" ^ user.username ^ "/block"; text="Block"; form=None}] in
  View.Profile.{
    user=View.User.{
      display_name=Option.value ~default:user.username user.display_name;
      username=user.username;
      profile_picture=
        Configuration.Url.user_profile_picture
          user.Database.LocalUser.profile_picture;
      self_link=
        Configuration.Url.user_path user.Database.LocalUser.username;
    };
    actions;
    followers=no_followers;
    following=no_following;
    details=[];
    details_source=""
  } 


(* * Actor *)
(* ** Edit (html) *)

let handle_actor_edit_get req =
  let username = Dream.param req "username" in
  let* current_user = get_user_checked req username in
  match current_user with
  | None ->
    redirect req "/feed"
  | Some current_user ->
    let token = Dream.csrf_token req in
    let* user = extract_user req current_user in
    let* headers, action = Navigation.build_navigation_bar req in
    tyxml @@
    View.Page.render_page (username ^ "'s Profile") [
      View.Header.render_header ?action headers;
      View.Profile.render_update_profile_box
        ~fields:["dream.csrf", token] user;
    ]

(* ** Edit (html) post  *)
let handle_actor_edit_post req =
  log.debug (fun f -> f "POST to actor edit");
  let username = Dream.param req "username" in
  let* current_user = get_user_checked req username in
  match current_user with
  | None ->
    redirect req "/feed"
  | Some current_user ->
    let username = Dream.param req "username" in
    let* data = (Dream.multipart req)
                |> sanitize_form_error ([%show: (string * (string option * string) list) list]) in
    match data with
    | _ when List.Assoc.mem ~eq:String.equal "avatar" data ->
      let* avatar =
        List.Assoc.get ~eq:String.equal "avatar" data
        |> lift_result_check extract_file_multipart_data
        |> Result.flat_map
             (lift_result_check
                (lift_pair_snd
                   (check_input_size "avatar" 3_000_000)))
        |> Lwt_result.lift in
      begin
        match avatar with
        | None -> redirect req "/feed"
        | Some (fname, avatar) ->
          log.debug (fun f -> f "got file %s, [%d]{%s..}" fname
                                (String.length avatar) (String.take 100 avatar));
          let* image = Images.upload_file req ~fname ~data:avatar in
          let* () =
            sql req
              (Database.LocalUser.update_profile_picture
                 ~id:current_user.Database.LocalUser.id
                 ~image) in
          redirect req (Configuration.Url.user_path username)
      end

    | _ when List.Assoc.mem ~eq:String.equal "display-name" data ||
             List.Assoc.mem ~eq:String.equal "about" data ->
      let* display_name =
        List.Assoc.get ~eq:String.equal "display-name" data
        |> lift_result_check extract_single_multipart_data
        |> Result.flat_map
             (lift_result_check (check_input_size "display-name" 80))
        |> Result.map (Option.filter (Fun.negate String.is_empty))
        |> Lwt_result.lift in
      let* about =
        List.Assoc.get ~eq:String.equal "about" data
        |> lift_result_check extract_single_multipart_data
        |> Result.flat_map
             (lift_result_check (check_input_size "about" 2048))
        |> Result.map (Option.filter (Fun.negate String.is_empty))
        |> Lwt_result.lift in
      let* () =
        match display_name with
        | None -> return_ok ()
        | Some display_name ->
          sql req
            (Database.LocalUser.update_display_name
               ~id:current_user.Database.LocalUser.id
               ~display_name) in
      let* () =
        match about with
        | None -> return_ok ()
        | Some about ->
          sql req
            (Database.LocalUser.update_about
               ~id:current_user.Database.LocalUser.id
               ~about) in
      redirect req (Configuration.Url.user_path username)
    | _ ->
      redirect req (Configuration.Url.user_path username)


(* ** Get (html) *)
let handle_actor_get_html req =
  let username = Dream.param req "username" in
  let* user =
    Dream.sql req
      (Database.LocalUser.find_user ~username)
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err))
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  (* let> _current_user = Common.with_current_user req in *)
  (* let* no_following, no_followers, _no_posts =
   *   Dream.sql req (fun db ->
   *     let* user = Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id) db in
   *     let* following = Database.Follows.count_following ~author:user db in
   *     let* followers = Database.Follows.count_followers ~target:user db in
   *     let* posts = Database.Posts.count_posts_by_author ~author:user db in
   *     Lwt.return_ok (following, followers,posts)
   *   ) |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in *)
  let timestamp = Dream.query req "start"
                  |> Fun.flip Option.bind (fun v -> Ptime.of_rfc3339 v |> Result.to_opt)
                  |> Option.map (fun (t, _, _) -> t)
                  |> Option.value ~default:(Ptime_clock.now ()) in
  let offset = Dream.query req "offset"
               |> Fun.flip Option.bind Int.of_string
               |> Option.value ~default:0 in
  let* profile = extract_user req user in

  let* contents =
    Dream.sql req begin fun db ->
      let* user = Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id) db in
      match Dream.query req "state" with
      | Some "followers" ->
        let* follows = 
          Database.Follows.collect_followers_for_actor
            ~since:timestamp ~offset:(offset * 10) ~limit:10 ~id:user db in
        let* follows =
          Lwt_list.map_s (fun follow ->
            let target = follow.Database.Follows.target_id in
            let* target = Database.Actor.resolve ~id:target db in
            Lwt.return_ok (follow, target)) follows
          >> Result.flatten_l in
        let* follows = Lwt_list.map_s (function
            follow, `Remote r ->
            let* remote =
              Database.RemoteUser.resolve ~id:r db in Lwt.return_ok (follow, `Remote remote)
          | follow, `Local l ->
            let* local =
              Database.LocalUser.resolve ~id:l db in Lwt.return_ok (follow, `Local local)
        ) follows >> Result.flatten_l in
        Lwt.return_ok (`Followers (timestamp, offset, follows))
      | Some "following" ->
        let* follows = 
          Database.Follows.collect_following_for_actor
            ~offset:(offset * 10) ~limit:10 ~since:timestamp ~id:user db in
        let* follows =
          Lwt_list.map_s (fun follow ->
            let target = follow.Database.Follows.target_id in
            let* target = Database.Actor.resolve ~id:target db in
            Lwt.return_ok (follow, target)) follows
          |> lift_pure in
        let* follows = Lwt.return @@ Result.flatten_l follows in
        Lwt.return_ok (`Following (timestamp, offset, follows))
      | Some "post" | _ ->
        let* posts =
          Database.Posts.collect_posts_by_author
            ~offset:(offset * 10) ~start_time:timestamp ~limit:10 ~author:user db in
        let* posts =
          Lwt_list.map_s (fun post ->
            let author = post.Database.Posts.author_id in
            let* author = Database.Actor.resolve ~id:author db in
            Lwt_result.return (author, post)
          ) posts
          |> lift_pure in
        let* posts = Lwt.return @@ Result.flatten_l posts in
        Lwt.return_ok (`Posts (timestamp, offset, posts))
    end
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let heading =
    let options =
      View.Utils.[
        {url=Format.sprintf "/users/%s?state=posts" username; text="Posts"; form=None};
        {url=Format.sprintf "/users/%s?state=followers" username; text="Followers"; form=None};
        {url=Format.sprintf "/users/%s?state=following" username; text="Following"; form=None};
      ] in
    match contents with
    | `Posts _ ->
      View.Components.render_heading ~icon:"1" ~current:"Posts" ~options ()
    | `Followers _ ->
      View.Components.render_heading ~icon:"2" ~current:"Followers" ~options ()
    | `Following _ ->
      View.Components.render_heading ~icon:"3" ~current:"Following" ~options () in
  let contents =
    match contents with
    | `Posts (_, _, _posts) ->
      (* View.Post_grid.render_post_grid posts *)
      assert false
    | `Followers _ ->
      assert false
    | `Following _ ->
      assert false in
  let* headers, action = Navigation.build_navigation_bar req in
  tyxml @@
  View.Page.render_page (username ^ "'s Profile") [
    View.Header.render_header ?action headers;
    View.Profile.render_profile profile;
    heading;
    contents;
  ]


(* ** Get (json) *)
let handle_actor_get_json req =
  let username = Dream.param req "username" in
  let* user =
    Dream.sql req
      (Database.LocalUser.find_user ~username)
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err))
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  let user_json = (user
                   |> Database.Interface.LocalUser.convert_to
                   |> Activitypub.Encode.person) in
  log.debug (fun f -> f "query for %s --> %a" username Yojson.Safe.pp user_json);
  activity_json user_json

(* ** Get *)
let handle_actor_get req =
  let content_type = Dream.header req "Accept"
                     |> Option.value ~default:(Activitypub.Constants.ContentType.html) in
  log.debug (fun f -> f "got user request for %s" content_type);
  match Activitypub.Constants.ContentType.of_string content_type with
  | None ->
    log.debug (fun f -> f "no idea for %s, returning html" content_type);
    Error_handling.handle_error_html
      (fun _ -> return @@ Error (`UnsupportedContentType content_type)) req
  | Some `HTML ->
    log.debug (fun f -> f "%s -> html; returning html" content_type);
    Error_handling.handle_error_html
      (handle_actor_get_html )req
  | Some `JSON ->
    log.debug (fun f -> f "%s -> json; returning json" content_type);
    Error_handling.handle_error_json
      (handle_actor_get_json) req

(* * Outbox *)
(* ** Get  *)
let handle_outbox_get req =
  Dream.log "GET %s/outbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

(* ** Post *)
(* let handle_outbox_post req =
 *   Dream.log "POST %s/outbox" (Dream.param req "username");
 *   let* body = Dream.body req in
 *   Dream.log "DATA: %s" body;
 *   Dream.respond ~status:`OK "" *)

(* * Followers *)
(* ** Get *)
let handle_followers_get req =
  let username = Dream.param req "username" in
  let* user = Dream.sql req (Database.LocalUser.find_user ~username)
              |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let* user = return @@ lift_opt ~else_:(fun _ -> `UserNotFound username)
                          user in
  let offset =
    let open Option in
    let* page = Dream.query req "page"
    and* start_time = Dream.query req "start" in
    let* page = Int.of_string page 
    and* start_time, _, _ = Ptime.of_rfc3339 start_time
                            |> Result.to_opt in
    return (page, start_time) in
  let is_page = Option.is_some offset in
  let offset, start_time = Option.value ~default:(0, Ptime_clock.now ())
                             offset in
  let* followers_collection_page =
    Dream.sql req
      (Ap_resolver.build_followers_collection_page
         start_time offset user) in
  let data =
    if is_page
    then Activitypub.Encode.ordered_collection_page (Decoders_yojson.Safe.Encode.string)
           followers_collection_page
    else Activitypub.Encode.ordered_collection (Decoders_yojson.Safe.Encode.string)
           ({
             id = Some (
               Configuration.Url.user_followers (user.Database.LocalUser.username)
               |> Uri.to_string
             );
             total_items=followers_collection_page.total_items
                         |> Option.get_exn_or "invalid assumption";
             contents=`First followers_collection_page; 
           } : string Activitypub.Types.ordered_collection) in
  activity_json data

(* * Following *)
(* ** Get *)
let handle_following_get req =
  let username = Dream.param req "username" in
  let* user = Dream.sql req (Database.LocalUser.find_user ~username)
              |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let* user = return @@ lift_opt ~else_:(fun _ -> `UserNotFound username)
                          user in
  let offset =
    let open Option in
    let* page = Dream.query req "page"
    and* start_time = Dream.query req "start" in
    let* page = Int.of_string page 
    and* start_time, _, _ = Ptime.of_rfc3339 start_time
                            |> Result.to_opt in
    return (page, start_time) in
  let is_page = Option.is_some offset in
  let offset, start_time = Option.value ~default:(0, Ptime_clock.now ())
                             offset in
  let* following_collection_page =
    Dream.sql req
      (Ap_resolver.build_following_collection_page
         start_time offset user) in
  let data =
    if is_page
    then Activitypub.Encode.ordered_collection_page (Decoders_yojson.Safe.Encode.string)
           following_collection_page
    else Activitypub.Encode.ordered_collection (Decoders_yojson.Safe.Encode.string)
           ({
             id = Some (
               Configuration.Url.user_following (user.Database.LocalUser.username)
               |> Uri.to_string
             );
             total_items=following_collection_page.total_items
                         |> Option.get_exn_or "invalid assumption";
             contents=`First following_collection_page; 
           } : string Activitypub.Types.ordered_collection) in
  activity_json data

(* * Users *)
(* ** Utils *)
let parse_user_types = function
  | "local" -> Some `Local
  | "remote" -> Some `Remote
  | _ -> None

let encode_user_types = function
  | `Local -> "local"
  | `Remote -> "remote"

let show_user_types = function
  | `Local -> "Local Users"
  | `Remote -> "Remote Users"


let classify_query s =
  let s = String.trim s in
  let has_spaces = String.contains s ' ' in
  let contains_at = (String.contains s '@') in
  if has_spaces || not contains_at
  then `SearchLike (String.split_on_char ' ' s)
  else begin
    let domain = String.split_on_char '@' s in
    match domain with
    | [username; domain] -> `Resolve (username, domain)
    | username :: _ -> `SearchLike [username]
    | _ -> `SearchLike [s]
  end

let render_users_page ?search_query:_ req user_type users =
  let* headers, action = Navigation.build_navigation_bar req in
  tyxml (View.Page.render_page (show_user_types user_type) [
    View.Header.render_header ?action headers;
    View.Components.render_heading
      ~icon:(match user_type with `Local -> "1" | _ -> "2")
      ~current:(show_user_types user_type) ~options:[
        {text="Local Users"; url="/users?type=" ^ encode_user_types `Local; form=None};
        {text="Remote Users"; url="/users?type=" ^ encode_user_types `Remote; form=None}
      ] ();
    View.User.render_users_search_box ();
    View.User.render_users_grid users;
  ])

(* ** Local users (get) *)
let handle_local_users_get req =
  let* current_user_link = current_user_link req in
  let offset_start =
    Dream.query req "offset-start"
    |> Option.flat_map Int.of_string
    |> Option.value ~default:0 in
  let limit = 10 in
  let search_query = Dream.query req "search" in
  let* users =
    match search_query with
    | Some query when not (String.is_empty query) ->
      let query = "%" ^ (String.replace ~sub:" " ~by:"%" query) ^ "%" in
      Dream.sql req (fun db ->
        Database.LocalUser.find_local_users
          ~offset:(offset_start * limit) ~limit
          ~pattern:query db)
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err))
    | _ ->
      Dream.sql req (fun db ->
        Database.LocalUser.collect_local_users
          ~offset:(offset_start * limit)
          ~limit:limit db)
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let* users_w_stats =
    Lwt_list.map_p (fun user ->
      Dream.sql req @@ fun db ->
      let* user_link = Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id) db in
      let* no_followers = Database.Follows.count_followers ~target:user_link db in
      let* no_posts = Database.Posts.count_posts_by_author ~author:user_link db in
      let* is_following =
        match current_user_link with
        | None -> return_ok None
        | Some current_user_link ->
          Database.Follows.is_following ~author:current_user_link ~target:user_link db
          |> Lwt_result.map Option.some in
      return_ok (user, no_followers, no_posts, is_following)
    ) users
    |> lift_pure
    >>= (fun r -> return (Result.flatten_l r))
    |> map_err (fun e -> `DatabaseError (Caqti_error.show e)) in
  let* current_user = current_user req in
  let can_follow =
    match current_user with
    | None -> fun _ -> false
    | Some current_user -> fun other_user ->
      not @@ String.equal
               (current_user.Database.LocalUser.username)
               (other_user.Database.LocalUser.username) in
  let users =
    List.map (fun (user, _no_followers, _no_posts, _is_following) ->
      let social = View.User.{
        followers=_no_followers;
        following=0;
        actions=(if can_follow user
                 then [View.Utils.{
                   text="Follow";
                   url=
                     Configuration.Url.user_follow_path user.username;
                   form=None                       
                 }]
                 else [])
      } in
      let user = View.User.{
        display_name=
          Option.value ~default:user.Database.LocalUser.username
            user.Database.LocalUser.display_name;
        username=user.Database.LocalUser.username;
        profile_picture=
          Configuration.Url.user_profile_picture  user.profile_picture;
        self_link =
          Configuration.Url.user_path user.username
      } in
      (user,social)
    ) users_w_stats in
  render_users_page ?search_query req `Local users

(* ** Remote users (get) *)
let handle_remote_users_get req =
  let* current_user_link = current_user_link req in
  let offset =
    Dream.query req "offset-start"
    |> Option.flat_map Int.of_string
    |> Option.value ~default:0 in
  let limit = 10 in
  let search_query = Dream.query req "search" in
  let* users =
    match search_query, current_user_link with
    | None, _ | Some _, None ->
      Dream.sql req (fun db ->
        Database.RemoteUser.collect_remote_users
          ~limit ~offset:(offset * limit) db)
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err))
    | Some query, Some _ ->
      match classify_query query with
      | `Resolve (user, domain) ->
        log.debug (fun f -> f "received explicit search - sending task to worker");
        Worker.send_task Worker.(SearchRemoteUser {
          username=user; domain=Some domain
        });
        let query = "%" ^ user ^ "%" in
        Dream.sql req (fun db ->
          Database.RemoteUser.find_remote_users ~limit
            ~offset:(offset * limit) ~pattern:query db
        )
        |> map_err (fun err -> `DatabaseError (Caqti_error.show err))
      | `SearchLike query ->
        log.debug (fun f -> f "received implicit search");
        begin match query with
        | [username] -> 
          log.debug (fun f -> f "implicit search over single parameter - sending task to worker");
          Worker.send_task Worker.(SearchRemoteUser {
            username; domain=None
          });
        | _ -> ()
        end;
        let query = "%" ^ (String.concat "%" query) ^ "%" in
        Dream.sql req (fun db ->
          Database.RemoteUser.find_remote_users ~limit
            ~offset:(offset * limit) ~pattern:query db
        )
        |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let* users_w_stats =
    Lwt_list.map_p (fun (url, user) ->
      Dream.sql req @@ fun db ->
      let* user_link = Database.Actor.create_remote_user ~remote_id:(user.Database.RemoteUser.id) db in
      let* no_followers = Database.Follows.count_followers ~target:user_link db in
      let* no_posts = Database.Posts.count_posts_by_author ~author:user_link db in
      let* is_following =
        match current_user_link with
        | None -> return_ok None
        | Some current_user_link ->
          let* follow =
            Database.Follows.find_follow_between
              ~author:current_user_link ~target:user_link db in
          return_ok @@ match follow with
          | Some follow when follow.Database.Follows.pending -> None
          | Some _ -> Some true
          | None -> Some false in
      return_ok (user, url, no_followers, no_posts, is_following)
    ) users
    |> lift_pure
    >>= (fun r -> return (Result.flatten_l r))
    |> map_err (fun e -> `DatabaseError (Caqti_error.show e)) in
  let users =
    List.map (fun (user, url, no_followers, _no_posts, _is_following) ->
      let fqn = (user.Database.RemoteUser.username) ^ "@" ^ url in
      let socials = View.User.{
        followers=no_followers;
        following=0;
        actions=[
          View.Utils.{text="Follow";url=("/users/" ^ fqn ^ "/follow"); form=None}
        ]
      } in
      let user = View.User.{
        display_name =
          Option.value ~default:user.Database.RemoteUser.username
            user.Database.RemoteUser.display_name;
        username=fqn;
        profile_picture=
          Option.value ~default:"/static/images/unknown.png"
            user.Database.RemoteUser.profile_picture;
        self_link=user.url;
      } in
      (user,socials)
    ) users_w_stats in
  render_users_page ?search_query req `Remote users

(* ** Users (get) *)
let handle_users_get req =
  let user_list_ty =
    Dream.query req "type"
    |> Option.flat_map parse_user_types
    |> Option.value ~default:`Local in
  match user_list_ty with
  | `Local ->
    handle_local_users_get req
  | `Remote ->
    handle_remote_users_get req

(* * Follow local *)
let handle_follow_local_user current_user username req =
  let* current_user =
    Dream.sql req (Database.Actor.create_local_user ~local_id:(current_user.Database.LocalUser.id))
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let* target_user =
    Dream.sql req
      (Database.LocalUser.find_user ~username)
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err))
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  let* target_user =
    Dream.sql req @@ Database.Actor.create_local_user ~local_id:(target_user.Database.LocalUser.id)
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  let* is_following =
    Dream.sql req @@ fun db ->
    Database.Follows.is_following ~author:current_user ~target:target_user db
    |> map_err (fun e -> `DatabaseError (Caqti_error.show e)) in
  match is_following with
  | false ->
    let id = Uuidm.v `V4 in
    let* _follow =
      Dream.sql req @@
      Database.Follows.create
        ~public_id:(Uuidm.to_string id)
        ~url:("/api/follows/" ^ (Uuidm.to_string id))
        ~author:current_user
        ~target:target_user
        ~pending:false
        ~created:(Ptime_clock.now ())
      |> map_err (fun e -> `DatabaseError (Caqti_error.show e)) in
    redirect req "/users"
  | true ->
    let* follow =
      Dream.sql req @@
      Database.Follows.find_follow_between
        ~author:current_user
        ~target:target_user
      |> map_err (fun e -> `DatabaseError (Caqti_error.show e)) in
    let* follow = Lwt.return (match follow with None -> Error (`DatabaseError "follow not found") | Some follow -> Ok follow) in
    let* _ =
      Dream.sql req @@
      Database.Follows.delete ~id:(follow.Database.Follows.id)
      |> map_err (fun e -> `DatabaseError (Caqti_error.show e)) in
    redirect req "/users"

(* * Follow remote *)
let handle_follow_remote_user current_user username domain req =
  Worker.send_task Worker.(
    FollowRemoteUser {
      user=current_user;
      username;
      domain
    }
  );
  redirect req "/users"

let handle_users_follow_post req =
  let username = Dream.param req "username" in
  let* current_user =
    let* current_user = current_user req in
    lift_opt ~else_:(fun _ -> `InvalidPermissions ("Attempt to follow user while logged out"))
      current_user
    |> return in
  match String.contains username '@', lazy (String.split_on_char '@' username) with
  | true, lazy (username :: domain)  ->
    let domain = String.concat "@" domain in
    handle_follow_remote_user current_user username domain req
  | _ ->
    handle_follow_local_user current_user username req

(* * Inbox (post) *)
let handle_inbox_post req =
  log.debug (fun f -> f ~request:req "got POST to inbox");
  log.debug (fun f -> f "validating request");
  let* valid_request =
    Http_sig.verify_request
      ~resolve_public_key:Ap_resolver.resolve_public_key req
    |> map_err (fun err -> `ResolverError err) in
  log.debug (fun f -> f "request validation completed with result %b" valid_request);
  let* () =
    if valid_request
    then return_ok ()
    else return (Error (`InvalidSignature)) in
  let username = Dream.param req "username" in
  let* _user = 
    Dream.sql req (Database.LocalUser.find_user ~username)
    |> map_err (fun err -> `DatabaseError (Caqti_error.show err))
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  let* body_text = lift_pure (Dream.body req) in
  Configuration.dump_string ~ty:"inbox-post" body_text;
  let* data =
    (Activitypub.Decode.(decode_string obj) body_text)
    |> (function
      | Ok _ as data -> data
      | Error err ->
        if Lazy.force Configuration.debug then
          IO.with_out ~flags:[Open_append; Open_creat] "ocamlot-failed-events.json" (Fun.flip IO.write_line (body_text ^ "\n"));
        Error err)
    |> return
    |> map_err (fun err -> `InvalidActivitypubObject err) in

  log.debug (fun f ->
    f "received activitypub object %a"
      Activitypub.Types.pp_obj data
  );
  log.debug (fun f ->
    f "headers were %s"
      ([%show: (string * string) list] (Dream.all_headers req))
  );

  let* _ =
    match[@warning "-27"] data with
    | `Accept { obj=`Follow { id=follow_id; _ }; id=accept_activity_id; raw=accept_obj; actor; _ } ->
      Worker.send_task Worker.(
        HandleAcceptFollow { follow_id }
      );
      return_ok ()
    | `Follow { id; actor; cc; to_; object_; state; raw } ->
      let lazy user_re = Configuration.Regex.local_user_id_format in
      let* re_matches = Re.exec_opt user_re object_
                        |> lift_opt ~else_:(fun () -> `InvalidData "follow target was not a valid user")
                        |> return in
      let username = Re.Group.get re_matches 1 in
      let* target = Dream.sql req (Database.LocalUser.find_user ~username)
                    |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
      let* target = lift_opt ~else_:(fun _ -> `UserNotFound object_) target
                    |> return in
      Worker.send_task Worker.(
        HandleRemoteFollow { id; actor; target; raw }
      );
      return_ok ()
    | `Undo { obj=`Follow { id=follow_id; _ }; _ } ->
      Worker.send_task Worker.(
        HandleUndoFollow { follow_id }
      );
      return_ok ()
    | `Create { obj=`Note note; actor; published; direct_message; _ } ->
      log.debug (fun f -> f "creating post");
      Worker.send_task Worker.(
        CreateRemoteNote { author=actor; direct_message; note }
      );
      return_ok ()
    | `Accept _
    | `Announce _
    | `Block _
    | `Note _
    | `Person _
    | `Undo _
    | `Delete _
    | `Create _ -> return_ok ()
    | `Like ({ id; published; actor; obj; raw; _ } as like) ->
      log.debug (fun f -> f "received like");
      let public_id = Configuration.extract_activity_id_from_url obj in
      let* public_id =
        lift_opt public_id
          ~else_:(fun _ ->
            `InvalidData "received like addressed to a post not from this instance.")
        |> return in
      let* post = Dream.sql req (Database.Posts.lookup_by_public_id ~public_id)
                  |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
      let* post =
        lift_opt post
          ~else_:(fun _ ->
            `ActivityNotFound "Received like for activity that is \
                               not present on this instance.")
        |> return in
      let published = Option.get_lazy Ptime_clock.now published in
      Worker.send_task Worker.(
        HandleRemoteLike { id; author=actor; target=post; published; raw_data=raw }
      );
      log.debug (fun f -> f "received like %a" Activitypub.Types.pp_like like);
      return_ok () in

  json (`Assoc [
    "ok", `List []
  ])

(* * Route *)
let route = 
  Dream.scope "/users" [] [
    Dream.get "" @@ Error_handling.handle_error_html (handle_users_get);
    Dream.get "/:username" @@ (handle_actor_get);

    Dream.get "/:username/edit" @@ Error_handling.handle_error_html (handle_actor_edit_get);
    Dream.post "/:username/edit" @@ Error_handling.handle_error_html (handle_actor_edit_post);

    Dream.post "/:username/follow" @@ Error_handling.handle_error_html (handle_users_follow_post);
    (* Dream.get "/:username/inbox" handle_inbox_get; *)
    Dream.post ":username/inbox" @@ Error_handling.handle_error_json (handle_inbox_post);
    Dream.get "/:username/outbox" handle_outbox_get;
    (* Dream.post "/:username/outbox" handle_outbox_post; *)

    Dream.get "/:username/followers" @@ Error_handling.handle_error_json (handle_followers_get);
    Dream.get "/:username/following" @@ Error_handling.handle_error_json (handle_following_get);
  ]
