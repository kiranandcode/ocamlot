[@@@warning "-33"]              (* like to open Lwt_result + Petrol + Tables *)


module Activity = struct

  type t = {
    id: string;
    raw_data: Yojson.Safe.t
  } [@@deriving show]

  let decode (id, (raw_data, ())) = {
    id;
    raw_data=Yojson.Safe.from_string raw_data
  }

  let create ~id ~data conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.insert ~table:Activity.table
      ~values:Expr.[
          Activity.id := s id;
          Activity.raw_data := s (Yojson.Safe.to_string data)
        ]
    |> Query.on_err `IGNORE
    |> Request.make_zero
    |> Petrol.exec conn

  let find_by_id ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
        Activity.id;
        Activity.raw_data
      ] ~from:Activity.table
    |> Query.where Expr.(Activity.id = s id)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let update_raw_data ~id ~raw_data conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:Activity.table
      ~set:Expr.[
          Activity.raw_data := s (Yojson.Safe.to_string raw_data)
        ]
    |> Query.where Expr.(Activity.id = s id)
    |> Query.on_err `IGNORE
    |> Request.make_zero
    |> Petrol.exec conn

end

module Actor = struct

  type t = {
    actor_id: int;
    link_id: [`Local of int | `Remote of int]
  }
  [@@deriving show]

  let lookup_local_user ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select [Actor.id]
      ~from:Actor.table
    |> Query.where Expr.(Actor.local_id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (id, ()) -> id)

  let lookup_remote_user ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select [Actor.id]
      ~from:Actor.table
    |> Query.where Expr.(Actor.remote_id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (id, ()) -> id)

  let resolve ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
        nullable Actor.local_id;
        nullable Actor.remote_id
      ] ~from:Actor.table
    |> Query.where Expr.(Actor.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (function
        | (Some local_id, (None, ())) -> `Local local_id
        | (None, (Some remote_id, ())) -> `Remote remote_id
        | (Some _, (Some _, ())) ->
          invalid_arg "found actor id resolving to both local and remote"
        | (None, (None, ())) ->
          invalid_arg "found actor id resolving to neither local or remote"
      )

  let create_local_user ~local_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:Actor.table
        ~values:Expr.[Actor.local_id := i local_id]
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    lookup_local_user ~id:local_id conn

  let create_remote_user ~remote_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:Actor.table
        ~values:Expr.[Actor.remote_id := i remote_id]
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    lookup_remote_user ~id:remote_id conn

end

module LocalUser = struct

  type private_key = X509.Private_key.t
  let pp_private_key fmt _ = Format.fprintf fmt "<opaque>"
  type public_key = X509.Public_key.t
  let pp_public_key fmt _ = Format.fprintf fmt "<opaque>"

  type t = {
    id: int;
    username: string;
    password: string;
    display_name: string option;
    about: string option;
    profile_picture: string option;
    manually_accepts_follows: bool;
    is_admin: bool;
    pubkey: public_key;
    privkey: private_key;
  }
  [@@deriving show]

  let decode =
    fun (id, (username, (password, (display_name, (about, (profile_picture, (manually_accepts_follows, (is_admin, (pubkey, (privkey, ())))))))))) ->
    {
      id;
      username;
      password;
      display_name;
      about;
      profile_picture;
      manually_accepts_follows;
      is_admin;
      pubkey=Result.get_ok (X509.Public_key.decode_pem (Cstruct.of_string pubkey));
      privkey=Result.get_ok (X509.Private_key.decode_pem (Cstruct.of_string privkey));
    }

  let find_user ~username conn =
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[
        LocalUser.id;
        LocalUser.username;
        LocalUser.password;
        nullable LocalUser.display_name;
        nullable LocalUser.about;
        nullable LocalUser.profile_picture;
        LocalUser.manually_accept_follows;
        LocalUser.is_admin;
        LocalUser.pubkey;
        LocalUser.privkey
      ]
      ~from:LocalUser.table
    |> Query.where Expr.(LocalUser.username = s username)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)


  let resolve ~id conn =
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[
        LocalUser.id;
        LocalUser.username;
        LocalUser.password;
        nullable LocalUser.display_name;
        nullable LocalUser.about;
        nullable LocalUser.profile_picture;
        LocalUser.manually_accept_follows;
        LocalUser.is_admin;
        LocalUser.pubkey;
        LocalUser.privkey
      ]
      ~from:LocalUser.table
    |> Query.where Expr.(LocalUser.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create_user ?display_name ?about ?(manually_accept_follows=false) ?(is_admin=false) ~username ~password conn =
    let create_actor local_id conn = Actor.create_local_user ~local_id conn in
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* password_hash =
      Lwt.return @@ (Password.hash ~pwd:password
                     |> Result.map_error (fun err -> `ArgonError err)) in
    let privkey = X509.Private_key.generate `RSA in
    let pubkey = X509.Private_key.public privkey in
    let* () =
      Query.insert ~table:LocalUser.table
        ~values:(Expr.[
          LocalUser.username := s username;
          LocalUser.password := s password_hash;
          LocalUser.manually_accept_follows := bl manually_accept_follows;
          LocalUser.is_admin := bl is_admin;
          LocalUser.pubkey := s (X509.Public_key.encode_pem pubkey |> Cstruct.to_string);
          LocalUser.privkey := s (X509.Private_key.encode_pem privkey |> Cstruct.to_string);
        ] @ (Option.map Expr.(fun about -> LocalUser.about := s about) about |> Option.to_list) 
                 @ (Option.map Expr.(fun display_name -> LocalUser.display_name := s display_name) display_name |> Option.to_list))
      |> Request.make_zero
      |> Petrol.exec conn in
    let* user = find_user ~username conn in
    let user = Option.get user in
    (* create an actor entry for the user *)
    let* _ = create_actor user.id conn in
    Lwt_result.return user


  let login_user ~username ~password conn =
    let open Lwt_result.Syntax in
    let* user = find_user ~username conn in
    match user with
    | None -> Lwt_result.return None
    | Some user ->
      let* password_is_correct =
        Lwt.return (Password.verify user.password ~pwd:password
                    |> Result.map_error (fun err -> `ArgonError err)) in
      Lwt_result.return
        (if password_is_correct
         then Some user
         else None)

  let update_password ~id ~password conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* password_hash =
      Lwt.return @@ (Password.hash ~pwd:password
                     |> Result.map_error (fun err -> `ArgonError err)) in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
        LocalUser.password := s password_hash
      ]
    |> Query.where Expr.(LocalUser.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let update_display_name ~id ~display_name conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
        LocalUser.display_name := s display_name
      ]
    |> Query.where Expr.(LocalUser.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let update_about ~id ~about conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
        LocalUser.about := s about
      ]
    |> Query.where Expr.(LocalUser.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let update_manually_accept_follows ~id ~manually_accept_follows conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
        LocalUser.manually_accept_follows := bl manually_accept_follows 
      ]
    |> Query.where Expr.(LocalUser.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn  

  let update_is_admin ~id ~is_admin conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
        LocalUser.is_admin := bl is_admin
      ]
    |> Query.where Expr.(LocalUser.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let update_profile_picture ~id ~image conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
        LocalUser.profile_picture := s image
      ]
    |> Query.where Expr.(LocalUser.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let collect_local_users ?(offset=0) ?(limit=10) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[
        LocalUser.id;
        LocalUser.username;
        LocalUser.password;
        nullable LocalUser.display_name;
        nullable LocalUser.about;
        nullable LocalUser.profile_picture;
        LocalUser.manually_accept_follows;
        LocalUser.is_admin;
        LocalUser.pubkey;
        LocalUser.privkey
      ]
      ~from:LocalUser.table
    |> Query.order_by ~direction:`ASC LocalUser.username
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let local_user_count conn =
    let open Petrol in
    let open Tables in
    Query.select Expr.[count_star] ~from:LocalUser.table
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let find_local_users ?(offset=0) ?(limit=10) ~pattern conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[
        LocalUser.id;
        LocalUser.username;
        LocalUser.password;
        nullable LocalUser.display_name;
        nullable LocalUser.about;
        nullable LocalUser.profile_picture;
        LocalUser.manually_accept_follows;
        LocalUser.is_admin;
        LocalUser.pubkey;
        LocalUser.privkey
      ]
      ~from:LocalUser.table
    |> Query.where Expr.(like LocalUser.username ~pat:(s pattern) ||
                         like LocalUser.display_name ~pat:(s pattern))
    |> Query.order_by ~direction:`ASC LocalUser.username
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let find_local_user_count ~pattern conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[count_star]
      ~from:LocalUser.table
    |> Query.where Expr.(like LocalUser.username ~pat:(s pattern) ||
                         like LocalUser.display_name ~pat:(s pattern))
    |> Query.order_by ~direction:`ASC LocalUser.username
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

end

module RemoteInstance = struct

  type t = {
    id: int;
    url: string;
    last_unreachable: Ptime.t option;
  }
  [@@deriving show]

  let decode (id, (url, (last_unreachable, ()))) =
    let last_unreachable =
      Option.map (fun s ->
          Ptime.of_rfc3339 s
          |> Result.get_ok
          |> (fun (t, _, _) -> t)
        ) last_unreachable in
    {
      id;
      url;
      last_unreachable
    }

  let lookup_instance ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
        RemoteInstance.id;
        RemoteInstance.url;
        nullable RemoteInstance.last_unreachable
      ] ~from:RemoteInstance.table
    |> Query.where Expr.(RemoteInstance.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let update_instance_last_unreachable ~id ~last_unreachable conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:RemoteInstance.table
      ~set:Expr.[
          RemoteInstance.last_unreachable :=
            s (Ptime.to_rfc3339 last_unreachable)
        ]
    |> Query.where
      Expr.(RemoteInstance.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let unset_instance_last_unreachable ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.update ~table:RemoteInstance.table
      ~set:Expr.[
          unset RemoteInstance.last_unreachable
        ]
    |> Query.where
      Expr.(RemoteInstance.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let resolve ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
        RemoteInstance.id;
        RemoteInstance.url;
        nullable RemoteInstance.last_unreachable
      ] ~from:RemoteInstance.table
    |> Query.where
      Expr.(RemoteInstance.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create_instance ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () = 
      Query.insert ~table:RemoteInstance.table
        ~values:Expr.[
            RemoteInstance.url := s url
          ]
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    let* instance = lookup_instance ~url conn in
    Lwt_result.return (Option.get instance)

  let find_possible_remote_instances_to_query
      ?(offset=0) ?(limit=10) pattern conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[RemoteInstance.id; RemoteInstance.url;
            nullable RemoteInstance.last_unreachable]
      ~from:RemoteInstance.table
    |> Query.where (
      Expr.exists begin
        Query.select
          Expr.[RemoteUser.id]
          ~from:RemoteUser.table
        |> Query.where Expr.(
            RemoteUser.instance_id = RemoteInstance.id &&
            (like RemoteUser.username ~pat:(s pattern) ||
             like RemoteUser.display_name ~pat:(s pattern))
          )
      end
    )
    |> Query.order_by RemoteInstance.url ~direction:`DESC
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

end

module UserImage = struct

  type t = {
    path: string;
    hash: string
  }

  let decode (path, (hash, ())) =  {path;hash}

  let find_by_hash ~hash conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[UserImage.path; UserImage.hash]
      ~from:UserImage.table
    |> Query.where Expr.(UserImage.hash = s hash)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let find_by_path ~path conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[UserImage.path; UserImage.hash]
      ~from:UserImage.table
    |> Query.where Expr.(UserImage.path = s path)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let resolve ~path conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[UserImage.path; UserImage.hash]
      ~from:UserImage.table
    |> Query.where Expr.(UserImage.path = s path)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create ~hash ~path conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:UserImage.table
        ~values:Expr.[
          UserImage.path := s path;
          UserImage.hash := s hash;
        ]
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    resolve ~path conn

end


module RemoteUser = struct

  type t = {
    id: int;
    username: string;
    instance_id: int;
    display_name: string option;
    url: string;
    inbox: string option;
    outbox: string option;
    followers: string option;
    following: string option;
    summary: string option;
    public_key_pem: string
  }
  [@@deriving show]

  let decode
      (id,
       (username,
        (instance_id,
         (display_name,
          (url,
           (inbox,
            (outbox,
             (followers,
              (following,
               (summary,
                (public_key_pem, ()))))))))))) = {
    id;
    username;
    instance_id;
    display_name;
    url;
    inbox;
    outbox;
    followers;
    following;
    summary;
    public_key_pem
  }

  let lookup_remote_user_by_address ~username ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let ri_url, ri_url_ref = Expr.(as_ RemoteInstance.url ~name:"ri_url") in
    let ri_id, ri_id_ref = Expr.(as_ RemoteInstance.id ~name:"ri_id") in
    Query.select
      Expr.[
        RemoteUser.id;
        RemoteUser.username;
        RemoteUser.instance_id;
        nullable RemoteUser.display_name;
        RemoteUser.url;
        nullable RemoteUser.inbox;
        nullable RemoteUser.outbox;
        nullable RemoteUser.followers;
        nullable RemoteUser.following;
        nullable RemoteUser.summary;
        RemoteUser.public_key_pem
      ]
      ~from:RemoteUser.table
    |> Query.join ~op:Query.INNER
      ~on:Expr.(ri_id_ref = RemoteUser.instance_id)
      (Query.select Expr.[ri_id; ri_url]
         ~from:RemoteInstance.table)
    |> Query.where Expr.(ri_url_ref = s url &&
                         RemoteUser.username = s username)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let lookup_remote_user_by_url ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[
        RemoteUser.id;
        RemoteUser.username;
        RemoteUser.instance_id;
        nullable RemoteUser.display_name;
        RemoteUser.url;
        nullable RemoteUser.inbox;
        nullable RemoteUser.outbox;
        nullable RemoteUser.followers;
        nullable RemoteUser.following;
        nullable RemoteUser.summary;
        RemoteUser.public_key_pem
      ]
      ~from:RemoteUser.table
    |> Query.where Expr.(RemoteUser.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let resolve ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[
        RemoteUser.id;
        RemoteUser.username;
        RemoteUser.instance_id;
        nullable RemoteUser.display_name;
        RemoteUser.url;
        nullable RemoteUser.inbox;
        nullable RemoteUser.outbox;
        nullable RemoteUser.followers;
        nullable RemoteUser.following;
        nullable RemoteUser.summary;
        RemoteUser.public_key_pem
      ]
      ~from:RemoteUser.table
    |> Query.where Expr.(RemoteUser.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create_remote_user
      ?display_name ?inbox ?outbox ?followers ?following ?summary
      ~username ~instance ~url ~public_key_pem conn =
    let create_remote remote_id conn = Actor.create_remote_user ~remote_id conn in
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:RemoteUser.table
        ~values:(
          Expr.[
            RemoteUser.username := s username;
            RemoteUser.instance_id := i instance;
          ] @
          (display_name |> Option.map Expr.(fun n ->
               RemoteUser.display_name := s n
             ) |> Option.to_list) @
          Expr.[RemoteUser.url := s url] @
          (inbox |> Option.map Expr.(fun n ->
               RemoteUser.inbox := s n
             ) |> Option.to_list) @
          (outbox |> Option.map Expr.(fun n ->
               RemoteUser.outbox := s n
             ) |> Option.to_list) @
          (followers |> Option.map Expr.(fun n ->
               RemoteUser.followers := s n
             ) |> Option.to_list) @
          (following |> Option.map Expr.(fun n ->
               RemoteUser.following := s n
             ) |> Option.to_list) @
          (summary |> Option.map Expr.(fun n ->
               RemoteUser.summary := s n
             ) |> Option.to_list) @
          Expr.[RemoteUser.public_key_pem := s public_key_pem]
        )
      |> Request.make_zero
      |> Petrol.exec conn in
    let* user = lookup_remote_user_by_url ~url conn in
    let user = Option.get user in
    (* create actor entry for remote user *)
    let* _ = create_remote user.id conn in
    Lwt_result.return user 

  let get_known_remote_actors
      ?(limit=10) ?(offset=0) conn = 
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let ri_url, ri_url_ref = Expr.as_ RemoteInstance.url ~name:"ri_url" in
    let ri_id, ri_id_ref = Expr.as_ RemoteInstance.id ~name:"ri_id" in
    Query.select
      Expr.[
        RemoteUser.username;
        ri_url_ref;
        RemoteUser.url
      ]
      ~from:RemoteUser.table
    |> Query.join
      ~op:INNER ~on:Expr.(RemoteUser.instance_id = ri_id_ref)
      (Query.select [ri_url; ri_id]
         ~from:RemoteInstance.table)
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (username, (i_url, (url, ()))) ->
        (username, i_url, url)
      ))


  let collect_remote_users ?(offset=0) ?(limit=10) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let ri_url, ri_url_ref = Expr.as_ RemoteInstance.url ~name:"ri_url" in
    let ri_id, ri_id_ref = Expr.as_ RemoteInstance.id ~name:"ri_id" in
    let ru_dn, ru_dn_ref = Expr.as_ RemoteUser.display_name ~name:"ru_dn" in
    Query.select
      Expr.[
        ri_url_ref;
        RemoteUser.id;
        RemoteUser.username;
        RemoteUser.instance_id;
        nullable ru_dn;
        RemoteUser.url;
        nullable RemoteUser.inbox;
        nullable RemoteUser.outbox;
        nullable RemoteUser.followers;
        nullable RemoteUser.following;
        nullable RemoteUser.summary;
        RemoteUser.public_key_pem
      ]
      ~from:RemoteUser.table
    |> Query.join ~op:INNER
      ~on:Expr.(ri_id_ref = RemoteUser.instance_id)
      (Query.select Expr.[
           ri_id;
           ri_url
         ] ~from:RemoteInstance.table)
    |> Query.order_by_ ~direction:`DESC
      Expr.[ru_dn_ref; ri_url_ref]
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (url, user) ->
        (url, decode user)
      ))

  let collect_remote_users_following
      ?(offset=0) ?(limit=10) ~target:target_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let a_id, a_id_ref = Expr.as_ Actor.id ~name:"a_id" in
    let a_rid, a_rid_ref = Expr.as_ Actor.remote_id ~name:"a_rid" in
    Query.select
      Expr.[
        RemoteUser.id;
        RemoteUser.username;
        RemoteUser.instance_id;
        nullable RemoteUser.display_name;
        RemoteUser.url;
        nullable RemoteUser.inbox;
        nullable RemoteUser.outbox;
        nullable RemoteUser.followers;
        nullable RemoteUser.following;
        nullable RemoteUser.summary;
        RemoteUser.public_key_pem
      ] ~from:RemoteUser.table
    |> Query.join ~op:INNER ~on:Expr.(a_rid_ref = RemoteUser.id)
      (Query.select [a_id; a_rid] ~from:Actor.table)
    |> Query.where
      (Expr.exists begin
          Query.select [Follows.id;Follows.author_id;Follows.target_id]
            ~from:Follows.table
          |> Query.where Expr.(
              Follows.author_id = a_id_ref &&
              Follows.target_id = i target_id &&
              Follows.pending = false_
            )
        end)
    |> Query.order_by RemoteUser.id ~direction:`ASC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let find_remote_users ?(offset=0) ?(limit=10) ~pattern conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let ri_url, ri_url_ref = Expr.as_ RemoteInstance.url ~name:"ri_url" in
    let ri_id, ri_id_ref = Expr.as_ RemoteInstance.id ~name:"ri_id" in
    Query.select
      Expr.[
        ri_url_ref;
        RemoteUser.id;
        RemoteUser.username;
        RemoteUser.instance_id;
        nullable RemoteUser.display_name;
        RemoteUser.url;
        nullable RemoteUser.inbox;
        nullable RemoteUser.outbox;
        nullable RemoteUser.followers;
        nullable RemoteUser.following;
        nullable RemoteUser.summary;
        RemoteUser.public_key_pem
      ]
      ~from:RemoteUser.table
    |> Query.join ~op:INNER
      ~on:Expr.(ri_id_ref = RemoteUser.instance_id)
      (Query.select Expr.[
           ri_id;
           ri_url
         ] ~from:RemoteInstance.table)
    |> Query.where Expr.(
        like RemoteUser.username ~pat:(s pattern) ||
        like RemoteUser.display_name ~pat:(s pattern)
      )
    |> Query.order_by_ ~direction:`DESC
      Expr.[RemoteUser.username; RemoteUser.display_name]
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (url, user) ->
        (url, decode user)
      ))

end

module Tag = struct

  type t = {
    id: int;
    name: string
  }
  [@@deriving show]

  let decode (id, (name, ())) =
    { id; name }

  let find_by_name ~tag conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select [Tag.id; Tag.name] ~from:Tag.table
    |> Query.where Expr.(Tag.name = s tag)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let resolve ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select [Tag.id; Tag.name] ~from:Tag.table
    |> Query.where Expr.(Tag.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create ~name conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:Tag.table
        ~values:Expr.[Tag.name := s name]
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    find_by_name ~tag:name conn

end

module Posts = struct

  type t = {
    id: int;
    public_id: string option;
    url: string;
    author_id: int;

    is_public: bool;
    is_follower_public: bool;

    summary: string option;
    content_type: [ `Markdown | `Org | `Text ];
    post_source: string;
    published: Ptime.t;

    raw_data: Yojson.Safe.t option;

  }
  [@@deriving show]

  let content_type_to_int = function `Markdown -> 0 | `Org -> 1 | `Text -> 2
  let int_to_content_type = function 0 -> `Markdown | 1 -> `Org | _ -> `Text

  let decode (id, (public_id, (url, (author_id, (is_public,
                                                 (is_follower_public, (summary, (content_type, 
                                                                                 (post_source, (published, (raw_data, ()))))))))))) =
    let published =
      Ptime.of_rfc3339 published
      |> Result.get_ok
      |> (fun (t, _, _) -> t) in
    let raw_data = Option.map Yojson.Safe.from_string raw_data in
    {
      id;
      public_id;
      url;
      author_id;
      is_public; is_follower_public;
      summary;
      content_type=int_to_content_type content_type;
      post_source;
      published;
      raw_data;
    }

  let lookup_by_public_id ~public_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Posts.id;
      nullable Posts.public_id;
      Posts.url;
      Posts.author_id;
      Posts.is_public;
      Posts.is_follower_public;
      nullable Posts.summary;
      Posts.content_type;
      Posts.post_source;
      Posts.published;
      nullable Posts.raw_data;
    ] ~from:Posts.table
    |> Query.where Expr.(Posts.public_id = s public_id)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let lookup_by_url ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Posts.id;
      nullable Posts.public_id;
      Posts.url;
      Posts.author_id;
      Posts.is_public;
      Posts.is_follower_public;
      nullable Posts.summary;
      Posts.content_type;
      Posts.post_source;
      Posts.published;
      nullable Posts.raw_data;
    ] ~from:Posts.table
    |> Query.where Expr.(Posts.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let resolve ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Posts.id;
      nullable Posts.public_id;
      Posts.url;
      Posts.author_id;
      Posts.is_public;
      Posts.is_follower_public;
      nullable Posts.summary;
      Posts.content_type;
      Posts.post_source;
      Posts.published;
      nullable Posts.raw_data;
    ] ~from:Posts.table
    |> Query.where Expr.(Posts.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create ?public_id ?summary ?raw_data 
        ?(is_public=true) ?(is_follower_public=true)
        ~url ~author ~post_content
        ~post_source ~published conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:Posts.table
        ~values:(
          (public_id
           |> Option.map Expr.(fun t -> Posts.public_id := s t)
           |> Option.to_list) @
          Expr.[
            Posts.url := s url;
            Posts.author_id := i author;
            Posts.is_public := bl is_public;
            Posts.is_follower_public := bl is_follower_public ] @
          (summary
           |> Option.map Expr.(fun t -> Posts.summary := s t)
           |> Option.to_list) @
          Expr.[
            Posts.content_type := i (content_type_to_int post_content);
            Posts.post_source := s post_source;
            Posts.published := s (Ptime.to_rfc3339 published);
          ] @
          (raw_data
           |> Option.map Expr.(fun t ->
             Posts.raw_data := s (Yojson.Safe.to_string t))
           |> Option.to_list)
        ) 
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    let* data = lookup_by_url ~url conn in
    Lwt_result.return (Option.get data)

  let count_posts_by_author ~author conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[count_star]
      ~from:Posts.table
    |> Query.where Expr.(Posts.author_id = i author)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (id, ()) -> id)

  let collect_posts_by_author
        ?(offset=0) ?(limit=10) ~start_time ~author conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let start_time = Ptime.to_rfc3339 start_time in
    Query.select Expr.[
      Posts.id;
      nullable Posts.public_id;
      Posts.url;
      Posts.author_id;
      Posts.is_public;
      Posts.is_follower_public;
      nullable Posts.summary;
      Posts.content_type;
      Posts.post_source;
      Posts.published;
      nullable Posts.raw_data;
    ] ~from:Posts.table
    |> Query.where
         Expr.(Posts.author_id = i author &&
               Posts.published <= s start_time &&
               Posts.is_public = true_)
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Query.order_by ~direction:`DESC Posts.published
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let post_to ?(offset=0) ?(limit=10) ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select [Posts.PostTo.actor_id]
      ~from:Posts.PostTo.table
    |> Query.where Expr.(Posts.PostTo.post_id = i id)
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> id))

  let post_cc ?(offset=0) ?(limit=10) ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select [Posts.PostCc.actor_id]
      ~from:Posts.PostCc.table
    |> Query.where Expr.(Posts.PostCc.post_id = i id)
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> id))

  let post_mentions ?(offset=0) ?(limit=10) ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select [Posts.PostMentions.actor_id]
      ~from:Posts.PostMentions.table
    |> Query.where Expr.(Posts.PostMentions.post_id = i id)
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> id))

  let post_tags ?(offset=0) ?(limit=10) ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let tag_name, tag_name_ref = Expr.as_ Tag.name ~name:"t_name" in
    let tag_id, tag_id_ref = Expr.as_ Tag.id ~name:"t_id" in
    Query.select [tag_name_ref]
      ~from:Posts.PostTags.table
    |> Query.join ~op:INNER (
      Query.select Expr.[tag_id; tag_name] ~from:Tag.table
    ) ~on:Expr.(tag_id_ref = Posts.PostTags.tag_id)
    |> Query.where Expr.(Posts.PostTags.post_id = i id)
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> id))

  let add_post_to ~id ~actor_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.insert ~table:Posts.PostTo.table
      ~values:Expr.[
        Posts.PostTo.post_id := i id;
        Posts.PostTo.actor_id := i actor_id;
      ]
    |> Query.on_err `IGNORE
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_tos ~id ~tos conn =
    Lwt_list.fold_left_s (fun acc actor_id -> match acc with
      | Ok () ->
        add_post_to ~id ~actor_id conn
      | Error _ -> Lwt.return acc
    ) (Ok ()) tos

  let add_post_cc ~id ~actor_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.insert ~table:Posts.PostCc.table
      ~values:Expr.[
        Posts.PostCc.post_id := i id;
        Posts.PostCc.actor_id := i actor_id;
      ]
    |> Query.on_err `IGNORE
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_ccs ~id ~ccs conn =
    Lwt_list.fold_left_s (fun acc actor_id -> match acc with
      | Ok () ->
        add_post_cc ~id ~actor_id conn
      | Error _ -> Lwt.return acc
    ) (Ok ()) ccs

  let add_post_mention ~id ~actor_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.insert ~table:Posts.PostMentions.table
      ~values:Expr.[
        Posts.PostMentions.post_id := i id;
        Posts.PostMentions.actor_id := i actor_id;
      ]
    |> Query.on_err `IGNORE
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_mentions ~id ~mentions conn =
    Lwt_list.fold_left_s (fun acc actor_id -> match acc with
      | Ok () ->
        add_post_mention ~id ~actor_id conn
      | Error _ -> Lwt.return acc
    ) (Ok ()) mentions

  let add_post_tag ~id ~tag conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let* tag = Tag.create ~name:tag conn in
    let open Tables in
    Query.insert ~table:Posts.PostTags.table
      ~values:Expr.[
        Posts.PostTags.post_id := i id;
        Posts.PostTags.tag_id := i tag.id;
      ]
    |> Query.on_err `IGNORE
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_tags ~id ~tags conn =
    Lwt_list.fold_left_s (fun acc tag -> match acc with
      | Ok () ->
        add_post_tag ~id ~tag conn
      | Error _ -> Lwt.return acc
    ) (Ok ()) tags

  (* a post is a direct message to actor(id) iff *)
  let is_direct_message ~id =
    let open Petrol in
    let open Tables in
    Expr.(
      (* if there is an entry in the to list   *)
      Expr.exists begin
        Query.select [Posts.PostTo.post_id; Posts.PostTo.actor_id]
          ~from:Posts.PostTo.table
        |> Query.where
             Expr.(
               (* for the post *)
               Posts.PostTo.post_id = Posts.id &&
               (* where we are the actor *)
               Posts.PostTo.actor_id = i id)
      end
      ||
      (* or, if there is an entry in the cc list *)
      Expr.exists begin
        Query.select [Posts.PostCc.post_id; Posts.PostCc.actor_id]
          ~from:Posts.PostCc.table
        |> Query.where
             Expr.(
               (* for the post *)
               Posts.PostCc.post_id = Posts.id &&
               (* where we are the actor *)
               Posts.PostCc.actor_id = i id)
      end) 

  let is_within_time ?end_time ~start_time () =
    let open Petrol in
    let open Tables in
    match end_time with
    | None ->
      Expr.(Posts.published <= s start_time)
    | Some end_time ->
      Expr.(Posts.published <= s start_time && s end_time <= Posts.published)

  let is_feed_post ~id =
    let open Petrol in
    let open Tables in
    Expr.(
      (
        (* TODO: We are not blocking/muting the author *)
        (* (1) we are the author *)
        Posts.author_id = i id ||
        (* we are following the author of the post && it is public *)
        (Expr.exists begin
           Query.select Expr.[Follows.author_id; Follows.target_id]
             ~from:Follows.table
           |> Query.where Expr.(
             Follows.author_id = i id &&
             Follows.target_id = Posts.author_id
           )
         end && (Posts.is_public || Posts.is_follower_public)
        ) ||
        (* it is a direct message to us *)
        is_direct_message ~id
      )
    )

  let is_local_post =  
    let open Petrol in
    let open Tables in
    Expr.exists begin
      Query.select [Actor.id; Actor.local_id] ~from:Actor.table
      |> Query.where Expr.(
        Actor.id = Posts.author_id &&
        Expr.is_not_null Actor.local_id
      )
    end

  let collect_feed ?(offset=0) ?(limit=10) ?start_time ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    (* collect posts where *)
    Query.select
      Expr.[
        Posts.id;
        nullable Posts.public_id;
        Posts.url;
        Posts.author_id;
        Posts.is_public;
        Posts.is_follower_public;
        nullable Posts.summary;
        Posts.content_type;
        Posts.post_source;
        Posts.published;
        nullable Posts.raw_data
      ] ~from:Posts.table
    |> Query.where Expr.(is_within_time ~start_time () && is_feed_post ~id)
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_feed ?start_time ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    (* collect posts where *)
    Query.select Expr.[ count_star ] ~from:Posts.table
    |> Query.where Expr.(is_within_time ~start_time () && is_feed_post ~id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let collect_direct ?(offset=0) ?(limit=10) ?start_time ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    (* collect posts where *)
    Query.select
      Expr.[
        Posts.id;
        nullable Posts.public_id;
        Posts.url;
        Posts.author_id;
        Posts.is_public;
        Posts.is_follower_public;
        nullable Posts.summary;
        Posts.content_type;
        Posts.post_source;
        Posts.published;
        nullable Posts.raw_data
      ] ~from:Posts.table
    |> Query.where
         Expr.(is_within_time ~start_time () &&
               is_direct_message ~id)
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_direct ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    (* collect posts where *)
    Query.select Expr.[ count_star ] ~from:Posts.table
    |> Query.where  Expr.(is_direct_message ~id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let collect_twkn ?(offset=0) ?(limit=10) ?start_time conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    (* collect posts where *)
    Query.select
      Expr.[
        Posts.id;
        nullable Posts.public_id;
        Posts.url;
        Posts.author_id;
        Posts.is_public;
        Posts.is_follower_public;
        nullable Posts.summary;
        Posts.content_type;
        Posts.post_source;
        Posts.published;
        nullable Posts.raw_data
      ] ~from:Posts.table
    |> Query.where
         Expr.(is_within_time ~start_time () &&
               Posts.is_public)
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_twkn conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    (* collect posts where *)
    Query.select Expr.[count_star] ~from:Posts.table
    |> Query.where
         Expr.(Posts.is_public)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let collect_local ?(offset=0) ?(limit=10) ?start_time conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    (* collect posts where *)
    Query.select
      Expr.[
        Posts.id;
        nullable Posts.public_id;
        Posts.url;
        Posts.author_id;
        Posts.is_public;
        Posts.is_follower_public;
        nullable Posts.summary;
        Posts.content_type;
        Posts.post_source;
        Posts.published;
        nullable Posts.raw_data
      ] ~from:Posts.table
    |> Query.where Expr.(is_within_time ~start_time () &&
                         Posts.is_public &&
                         is_local_post)
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_local conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    (* collect posts where *)
    Query.select Expr.[count_star] ~from:Posts.table
    |> Query.where
         Expr.(Posts.is_public && is_local_post)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

end

module Follows = struct

  type t = {
    id: int;
    public_id: string option;
    url: string;
    raw_data: Yojson.Safe.t option;
    pending: bool;
    created: Ptime.t;
    updated: Ptime.t option;
    author_id: int;
    target_id: int;
  } [@@deriving show]

  let decode (id, (public_id, (url, (raw_data, (pending, (created, (updated, (author_id, (target_id, ()))))))))) =
    let raw_data = Option.map Yojson.Safe.from_string raw_data in
    let from_string s = Ptime.of_rfc3339 s |> Result.get_ok |> (fun (time, _, _) -> time) in
    let created = from_string created in
    let updated = Option.map from_string updated in
    {
      id;
      public_id;
      url;
      raw_data;
      pending;
      created;
      updated;
      author_id;
      target_id;
    }

  let lookup_by_public_id ~public_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Follows.id;
      nullable Follows.public_id;
      Follows.url;
      nullable Follows.raw_data;
      Follows.pending;
      Follows.created;
      nullable Follows.updated;
      Follows.author_id;
      Follows.target_id;
    ] ~from:Follows.table
    |> Query.where Expr.(Follows.public_id = s public_id)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let lookup_by_url ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Follows.id;
      nullable Follows.public_id;
      Follows.url;
      nullable Follows.raw_data;
      Follows.pending;
      Follows.created;
      nullable Follows.updated;
      Follows.author_id;
      Follows.target_id;
    ] ~from:Follows.table
    |> Query.where Expr.(Follows.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let resolve ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Follows.id;
      nullable Follows.public_id;
      Follows.url;
      nullable Follows.raw_data;
      Follows.pending;
      Follows.created;
      nullable Follows.updated;
      Follows.author_id;
      Follows.target_id;
    ] ~from:Follows.table
    |> Query.where Expr.(Follows.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create ?public_id ?raw_data ?updated ~url ~author ~target ~pending ~created conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:Follows.table ~values:(
        Expr.[
          Follows.url := s url;
          Follows.pending := bl pending;
          Follows.created := s (Ptime.to_rfc3339 created);
          Follows.author_id := i author;
          Follows.target_id := i target;
        ] 
        @ (Option.map Expr.(fun public_id -> Follows.public_id := s public_id) public_id |> Option.to_list)
        @ (Option.map Expr.(fun raw_data -> Follows.raw_data := s (Yojson.Safe.to_string raw_data)) raw_data |> Option.to_list)
        @ (Option.map Expr.(fun updated -> Follows.updated := s (Ptime.to_rfc3339 updated)) updated |> Option.to_list)
      )
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    let* result = lookup_by_url ~url conn in
    Lwt_result.return (Option.get result)

  let update_pending_status ~id ~pending conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let updated = Ptime_clock.now () in
    Query.update ~table:Follows.table
      ~set:Expr.[
        Follows.pending := bl pending;
        Follows.updated := s (Ptime.to_rfc3339 updated);
      ]
    |> Query.where Expr.(Follows.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let delete ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.delete ~from:Follows.table
    |> Query.where Expr.(Follows.id = i id)
    |> Request.make_zero
    |> Petrol.exec conn

  let collect_follows_for_actor ?(offset=0) ?(limit=10) ?since ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let since = Option.value ~default:(Ptime_clock.now ()) since |> Ptime.to_rfc3339 in
    Query.select Expr.[
      Follows.id;
      nullable Follows.public_id;
      Follows.url;
      nullable Follows.raw_data;
      Follows.pending;
      Follows.created;
      nullable Follows.updated;
      Follows.author_id;
      Follows.target_id;
    ] ~from:Follows.table
    |> Query.where Expr.(
      (Follows.target_id = i id || Follows.author_id = i id) &&
      (coalesce [Follows.updated; Follows.created]) <= s since &&
      Follows.pending = true_
    )
    |> Query.order_by Expr.(coalesce [Follows.updated; Follows.created]) ~direction:`DESC
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let is_following ~author ~target conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[count_star] ~from:Follows.table
    |> Query.where Expr.(Follows.author_id = i author && Follows.target_id = i target && Follows.pending = false_)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (c, ()) -> c > 0)

  let find_follow_between ~author ~target conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Follows.id;
      nullable Follows.public_id;
      Follows.url;
      nullable Follows.raw_data;
      Follows.pending;
      Follows.created;
      nullable Follows.updated;
      Follows.author_id;
      Follows.target_id;
    ] ~from:Follows.table
    |> Query.where Expr.(Follows.author_id = i author && Follows.target_id = i target)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let count_following ~author conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[count_star] ~from:Follows.table
    |> Query.where Expr.(Follows.author_id = i author && Follows.pending = false_)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (c, ()) -> c)

  let collect_following_for_actor ?(offset=0) ?(limit=10) ?since ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let since = Option.value ~default:(Ptime_clock.now ()) since |> Ptime.to_rfc3339 in
    Query.select Expr.[
      Follows.id;
      nullable Follows.public_id;
      Follows.url;
      nullable Follows.raw_data;
      Follows.pending;
      Follows.created;
      nullable Follows.updated;
      Follows.author_id;
      Follows.target_id;
    ] ~from:Follows.table
    |> Query.where Expr.(
      (Follows.author_id = i id) &&
      (coalesce [Follows.updated; Follows.created]) <= s since &&
      Follows.pending = false_
    )
    |> Query.order_by Expr.(coalesce [Follows.updated; Follows.created]) ~direction:`DESC
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_followers ~target conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[count_star] ~from:Follows.table
    |> Query.where Expr.(Follows.target_id = i target && Follows.pending = false_)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (c, ()) -> c)

  let collect_followers_for_actor ?(offset=0) ?(limit=10) ?since ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let since = Option.value ~default:(Ptime_clock.now ()) since |> Ptime.to_rfc3339 in
    Query.select Expr.[
      Follows.id;
      nullable Follows.public_id;
      Follows.url;
      nullable Follows.raw_data;
      Follows.pending;
      Follows.created;
      nullable Follows.updated;
      Follows.author_id;
      Follows.target_id;
    ] ~from:Follows.table
    |> Query.where Expr.(
      (Follows.target_id = i id) &&
      (coalesce [Follows.updated; Follows.created]) <= s since &&
      Follows.pending = false_
    )
    |> Query.order_by Expr.(coalesce [Follows.updated; Follows.created]) ~direction:`DESC
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

end

module Likes = struct

  type t = {
    id: int;
    public_id: string option;
    url: string;
    raw_data: Yojson.Safe.t option;
    published: Ptime.t;
    post_id: int;
    actor_id: int;
  }
  [@@deriving show]

  let decode (id, (public_id, (url, (raw_data, (published, (post_id, (actor_id, ()))))))) =
    let raw_data = Option.map Yojson.Safe.from_string raw_data in
    let from_string s = Ptime.of_rfc3339 s |> Result.get_ok |> (fun (time, _, _) -> time) in
    let published = from_string published in
    {
      id;
      public_id;
      url;
      raw_data;
      published;
      post_id;
      actor_id;
    }

  let resolve ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Likes.id;
      nullable Likes.public_id;
      Likes.url;
      nullable Likes.raw_data;
      Likes.published;
      Likes.post_id;
      Likes.actor_id;
    ] ~from:Likes.table
    |> Query.where Expr.(Likes.id = i id)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let lookup_by_url ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Likes.id;
      nullable Likes.public_id;
      Likes.url;
      nullable Likes.raw_data;
      Likes.published;
      Likes.post_id;
      Likes.actor_id;
    ] ~from:Likes.table
    |> Query.where Expr.(Likes.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let create ?public_id ?raw_data ~url ~post ~actor ~published conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    let* () =
      Query.insert ~table:Likes.table ~values:(
        Expr.[
          Likes.url := s url;
          Likes.published := s (Ptime.to_rfc3339 published);
          Likes.post_id := i post;
          Likes.actor_id := i actor;
        ] 
        @ (Option.map Expr.(fun public_id -> Likes.public_id := s public_id) public_id |> Option.to_list)
        @ (Option.map Expr.(fun raw_data -> Likes.raw_data := s (Yojson.Safe.to_string raw_data)) raw_data |> Option.to_list)
      )
      |> Query.on_err `IGNORE
      |> Request.make_zero
      |> Petrol.exec conn in
    let* result = lookup_by_url ~url conn in
    Lwt_result.return (Option.get result)

  let collect_for_post ?(offset=0) ?(limit=10) ~post conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Likes.id;
      nullable Likes.public_id;
      Likes.url;
      nullable Likes.raw_data;
      Likes.published;
      Likes.post_id;
      Likes.actor_id;
    ] ~from:Likes.table
    |> Query.where Expr.(Likes.post_id = i post)
    |> Query.order_by Likes.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let collect_by_actor ?(offset=0) ?(limit=10) ~actor conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Tables in
    Query.select Expr.[
      Likes.id;
      nullable Likes.public_id;
      Likes.url;
      nullable Likes.raw_data;
      Likes.published;
      Likes.post_id;
      Likes.actor_id;
    ] ~from:Likes.table
    |> Query.where Expr.(Likes.actor_id = i actor)
    |> Query.order_by Likes.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

end
