open Containers
open Common

let req_post ~headers url body =
  let body = Cohttp_lwt.Body.of_string body in
  try
    let+ pair =
      Cohttp_lwt_unix.Client.post
        ~headers
        ~body
        url in
    Lwt_result.return pair
  with exn -> Lwt.return (Result.of_exn exn)

let req ~headers url =
  try
    let+ pair =
      Cohttp_lwt_unix.Client.get
        ~headers:(Cohttp.Header.of_list headers)
        url in
    Lwt_result.return pair
  with exn -> Lwt.return (Result.of_exn exn)

let signed_req f (key_id, priv_key) uri body_str =
  let current_time = Ptime_clock.now () in
  let headers =
    Http_sig.build_signed_headers
      ~current_time ~method_:"POST" ~body_str
      ~headers:(Http_sig.StringMap.of_list [
        "Content-Type", APConstants.ContentType.ld_json_activity_streams
      ]) ~key_id ~priv_key ~uri
    |> Cohttp.Header.of_list in
  f ~headers uri body_str

let signed_post key uri body =
  signed_req req_post key uri body

let activity_req ?(headers=[]) url =
  let activity_header =
    ("Accept", APConstants.ContentType.activity_json) in
  req ~headers:(activity_header :: headers) url

let json_rd_req ?(headers=[]) url =
  let json_rd_header =
    ("Accept", APConstants.Webfinger.json_rd) in
  req ~headers:(json_rd_header :: headers) url

let lookup_request url =
  (* NOTE: Not obvious, but you need to specify accept headers, else pleroma will return html *)
  let+! (_resp, body) = activity_req (Uri.of_string url) in
  let+! actor = Cohttp_lwt.Body.to_string body
                |> Lwt.map Activitypub.Decode.(decode_string person) in
  let pub_key =
    actor.public_key.pem
    |> Cstruct.of_string
    |> X509.Public_key.decode_pem
    |> Result.map_err (fun (`Msg err) -> err) in
  Lwt.return pub_key

let resolve_remote_user_with_webfinger ~local_lookup ~webfinger_uri db
  : (Database.RemoteUser.t, string) Lwt_result.t =
  let+! domain = Uri.host webfinger_uri |> Result.of_opt |> Lwt.return in
  let extract_self_link query =
    query.Activitypub.Types.Webfinger.links
    |> List.find_map (function
        Activitypub.Types.Webfinger.Self (
          (`ActivityJson | `ActivityJsonLd | `Json), url
        ) -> Some (Uri.of_string url)
      | _ -> None)
    |> Result.of_opt
    |> Lwt.return in
  let+! result = local_lookup db in
  match result with
    Some v -> Lwt.return_ok v
  | None ->
    (* remote user not found *)
    (* webfinger to find user url *)
    let+! remote_user_url =
      let+! (_, body) = json_rd_req webfinger_uri in
      let+ body = Cohttp_lwt.Body.to_string body in
      let+! query_res = body
                        |> Activitypub.Decode.(decode_string Webfinger.query_result)
                        |> Lwt.return in
      extract_self_link query_res in
    (* retrieve json *)
    let+! (_, body) = activity_req remote_user_url in
    let+ body = Cohttp_lwt.Body.to_string body in
    let+! person_res = body
                       |> Activitypub.Decode.(decode_string person)
                       |> Lwt.return in
    let+! remote_instance = Database.RemoteInstance.create_instance domain db in
    let+! () = Database.RemoteInstance.record_instance_reachable remote_instance db in
    let+! username = person_res.preferred_username
                     |> Result.of_opt
                     |> Lwt.return in
    let+! url = person_res.url
                |> Result.of_opt
                |> Lwt.return in
    Database.RemoteUser.create_remote_user
      ?display_name:person_res.name
      ~inbox:person_res.inbox
      ~outbox:person_res.outbox
      ?followers:person_res.followers
      ?following:person_res.following
      ?summary:person_res.summary
      ~public_key_pem:person_res.public_key.pem
      ~username
      ~instance:(Database.RemoteInstance.self remote_instance)
      ~url:url db

let resolve_remote_user ~username ~domain db =
  resolve_remote_user_with_webfinger
    ~local_lookup:(Database.RemoteUser.lookup_remote_user_by_address ~username ~domain)
    ~webfinger_uri:(Uri.make
                      ~scheme:"https"
                      ~host:domain
                      ~path:"/.well-known/webfinger"
                      ~query:["resource", [Printf.sprintf "acct:%s@%s" username domain]] ()
                   ) db

let resolve_remote_user_by_url url db =
  let url' = Uri.to_string url in
  resolve_remote_user_with_webfinger
    ~local_lookup:(Database.RemoteUser.lookup_remote_user_by_url url')
    ~webfinger_uri:(
      url
      |> Fun.flip Uri.with_path "/.well-known/webfinger"
      |> Fun.flip Uri.with_query' ["resource", url']
      |> Fun.flip Uri.with_scheme (Some "https")
    ) db
  
let create_accept_follow config follow remote local db =
  let local_user =
    Configuration.Url.user config (Database.LocalUser.username local)
    |> Uri.to_string in
  let id = Database.Activity.fresh_id () in
  let accept = 
  ({
    id=Configuration.Url.activity_endpoint config (Database.Activity.id_to_string id) |> Uri.to_string;
    actor=local_user;
    published=Some (Ptime_clock.now ());
    obj=({
      id=Database.Follow.url follow;
      actor=Database.RemoteUser.url remote;
      cc=[]; to_=[local_user]; state=None; raw=`Null;
      object_=local_user;
    }: Activitypub.Types.follow); raw=`Null;
  }:_ Activitypub.Types.accept) in
  let accept = Activitypub.Encode.(accept follow) accept in
  let+ _ = Database.Activity.create ~id ~data:accept db in
  Lwt_result.return accept

let build_follow_request config local remote db =
  let id = Database.Activity.fresh_id () in
  let local_actor_url = 
    Database.LocalUser.username local
    |> Configuration.Url.user config
    |> Uri.to_string in
  let remote_actor_url = Database.RemoteUser.url remote in
  let follow_request =
    let id =
      Database.Activity.id_to_string id
      |> Configuration.Url.activity_endpoint config
      |> Uri.to_string in
    Activitypub.Types.{
      id; actor=local_actor_url;
      cc = []; to_ = [ remote_actor_url ];
      object_=remote_actor_url; state = Some `Pending;
      raw=`Null
    }  in
  let data = Activitypub.Encode.follow follow_request in
  let+! _ =
    let+! author = Database.Actor.of_local (Database.LocalUser.self local) db in
    let+! target = Database.Actor.of_remote (Database.RemoteUser.self remote) db in
    Database.Follow.create_follow
      ~url:(Configuration.Url.activity_endpoint config (Database.Activity.id_to_string id)
            |> Uri.to_string)
      ~public_id:(Database.Activity.id_to_string id)
      ~author ~target ~pending:true
      ~created:(CalendarLib.Calendar.now ()) db in
  let+! _ = Database.Activity.create ~id ~data db in
  Lwt_result.return (data |> Yojson.Safe.to_string)

let follow_remote_user config
      (local: Database.LocalUser.t)
      ~username ~domain db: (unit,string) Lwt_result.t =
  let+! remote = resolve_remote_user ~username ~domain db in
  let+! follow_request = build_follow_request config local remote db in
  let uri = Database.RemoteUser.inbox remote in
  let key_id =
    Database.LocalUser.username local
    |> Configuration.Url.user_key config
    |> Uri.to_string in
  let priv_key =
    Database.LocalUser.privkey local in
  let+! resp, _  = signed_post (key_id, priv_key) uri follow_request in
  match resp.status with
  | `OK -> Lwt_result.return ()
  | _ -> Lwt_result.fail "request failed"

let accept_remote_follow config follow remote local db =
  let+! accept_follow = create_accept_follow config follow remote local db in
  let uri = Database.RemoteUser.inbox remote in
  let key_id =
    Database.LocalUser.username local
    |> Configuration.Url.user_key config
    |> Uri.to_string in
  let priv_key =
    Database.LocalUser.privkey local in
  print_endline @@ Printf.sprintf "sending accept follow request to %s\n\ndata is %s"
                     (Uri.to_string uri)
                     (Yojson.Safe.pretty_to_string accept_follow) ;

  let+! resp, body  = signed_post (key_id, priv_key) uri
                     (Yojson.Safe.to_string accept_follow) in
  let+ _ = Cohttp_lwt.Body.to_string body in
  match resp.status with
  | `OK ->
    let+! () =
      Database.Follow.update_follow_pending_status ~timestamp:(CalendarLib.Calendar.now ())
        (Database.Follow.self follow) false db in
    Lwt_result.return ()
  | _ -> Lwt_result.fail "request failed"


let accept_local_follow _config follow ~target:remote ~author:local db =
  let+! () =
    let+! follow_remote = Database.Link.resolve (Database.Follow.target follow) db
      >>= function Database.Actor.Remote r -> Lwt.return_ok (Database.RemoteUser.url r)
                 | _ -> Lwt.return_error "invalid user" in
    if String.equal (Database.RemoteUser.url remote) follow_remote
    then Lwt.return_ok ()
    else Lwt.return_error "inconsistent follow" in
  let+! () =
    let+! follow_local = Database.Link.resolve (Database.Follow.author follow) db
      >>= function Database.Actor.Local l -> Lwt.return_ok (Database.LocalUser.username l)
                 | _ -> Lwt.return_error "invalid user" in
    if String.equal (Database.LocalUser.username local) follow_local
    then Lwt.return_ok ()
    else Lwt.return_error "inconsistent follow" in
  let+ _ =
    Database.Follow.update_follow_pending_status ~timestamp:(CalendarLib.Calendar.now ())
      (Database.Follow.self follow) false db in
  Lwt_result.return ()


let follow_local_user config follow_url remote_url local_user data db =
  let+! remote = resolve_remote_user_by_url (Uri.of_string remote_url) db in
  let+! follow = 
    let+! author =
      Database.Actor.of_remote (Database.RemoteUser.self remote) db in
    let+! target =
      Database.Actor.of_local (Database.LocalUser.self local_user) db in
    Database.Follow.create_follow
      ~raw_data:(Yojson.Safe.to_string data)
      ~url:follow_url ~author ~target ~pending:true
      ~created:(CalendarLib.Calendar.now ()) db in
  let+! () =
    if not @@ Database.LocalUser.manually_accept_follows local_user
    then accept_remote_follow config follow remote local_user db
    else Lwt_result.return () in
  Lwt_result.return ()

