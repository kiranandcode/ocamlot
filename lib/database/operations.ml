[@@@warning "-33"]              (* like to open Lwt_result + Petrol + Tables *)

module LocalUser = struct

  type t = {
    id: int;
    username: string;
    password: string;
    display_name: string option;
    about: string option;
    manually_accepts_follows: bool;
    is_admin: bool;
    pubkey: X509.Public_key.t;
    privkey: X509.Private_key.t;
  }

  let decode =
    fun (id, (username, (password, (display_name, (about, (manually_accepts_follows, (is_admin, (pubkey, (privkey, ()))))))))) ->
    {
      id;
      username;
      password;
      display_name;
      about;
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


  let resolve_user ~id conn =
    let open Petrol in
    let open Tables in
    Query.select
      Expr.[
        LocalUser.id;
        LocalUser.username;
        LocalUser.password;
        nullable LocalUser.display_name;
        nullable LocalUser.about;
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
    find_user ~username conn


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

end

module RemoteInstance = struct

  type t = {
    id: int;
    url: string;
    last_unreachable: Ptime.t option;
  }

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

  let resolve_instance ~id conn =
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

end
