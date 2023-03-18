[@@@warning "-33-32"]              (* like to open Lwt_result + Petrol + Tables *)

open (struct
  module TypesInner : sig
    type activity_id = private string
    [@@deriving show]
    val of_string : string -> activity_id

    type local_user_id = private int64
    [@@deriving show]
    val local_of_int64 : int64 -> local_user_id
    val unsafe_local_of_string : string -> local_user_id

    type remote_user_id = private int64
    [@@deriving show]
    val remote_of_int64 : int64 -> remote_user_id
    val unsafe_remote_of_string : string -> remote_user_id

    type actor_id = private int64
    [@@deriving show]
    val actor_of_int64 : int64 -> actor_id
    val unsafe_actor_of_string : string -> actor_id

    type remote_instance_id = private int64
    [@@deriving show]
    val remote_instance_of_int64 : int64 -> remote_instance_id
    val unsafe_remote_instance_of_string : string -> remote_instance_id

    type post_id = private int64
    [@@deriving show]
    val post_of_int64 : int64 -> post_id
    val unsafe_post_of_string : string -> post_id

    type like_id = private int64
    [@@deriving show]
    val like_of_int64 : int64 -> like_id
    val unsafe_like_of_string : string -> like_id

    type reboost_id = private int64
    [@@deriving show]
    val reboost_of_int64 : int64 -> reboost_id
    val unsafe_reboost_of_string : string -> reboost_id

    type follow_id = private int64
    [@@deriving show]
    val follow_of_int64 : int64 -> follow_id
    val unsafe_follow_of_string : string -> follow_id

    type tag_id = private int64
    [@@deriving show]
    val tag_of_int64 : int64 -> tag_id
    val unsafe_tag_of_string : string -> tag_id

  end = struct
    type activity_id = string
    [@@deriving show]
    let of_string : string -> activity_id = fun s -> s

    type local_user_id = int64
    [@@deriving show]
    let local_of_int64 : int64 -> local_user_id = fun s -> s
    let unsafe_local_of_string s = Int64.of_string s

    type remote_user_id = int64
    [@@deriving show]
    let remote_of_int64 : int64 -> remote_user_id = fun s -> s
    let unsafe_remote_of_string s = Int64.of_string s

    type actor_id = int64
    [@@deriving show]
    let actor_of_int64 : int64 -> actor_id = fun s -> s
    let unsafe_actor_of_string s = Int64.of_string s

    type remote_instance_id = int64
    [@@deriving show]
    let remote_instance_of_int64 : int64 -> remote_instance_id = fun s -> s
    let unsafe_remote_instance_of_string s = Int64.of_string s


    type post_id = int64
    [@@deriving show]
    let post_of_int64 : int64 -> post_id = fun s -> s
    let unsafe_post_of_string s = Int64.of_string s

    type like_id = int64
    [@@deriving show]
    let like_of_int64 : int64 -> like_id = fun s -> s
    let unsafe_like_of_string s = Int64.of_string s

    type reboost_id = int64
    [@@deriving show]
    let reboost_of_int64 : int64 -> reboost_id = fun s -> s
    let unsafe_reboost_of_string s = Int64.of_string s

    type follow_id = int64
    [@@deriving show]
    let follow_of_int64 : int64 -> follow_id = fun s -> s
    let unsafe_follow_of_string s = Int64.of_string s

    type tag_id = int64
    [@@deriving show]
    let tag_of_int64 : int64 -> tag_id = fun s -> s
    let unsafe_tag_of_string s = Int64.of_string s

  end
end)

module Types = (TypesInner : sig
                  type activity_id = TypesInner.activity_id [@@deriving show]
                  type local_user_id = TypesInner.local_user_id [@@deriving show]
                  type remote_user_id = TypesInner.remote_user_id [@@deriving show]
                  type actor_id = TypesInner.actor_id [@@deriving show]
                  type remote_instance_id = TypesInner.remote_instance_id [@@deriving show]
                  type post_id = TypesInner.post_id [@@deriving show]
                  type like_id = TypesInner.like_id [@@deriving show]
                  type reboost_id = TypesInner.reboost_id [@@deriving show]
                  type follow_id = TypesInner.follow_id [@@deriving show]
                  type tag_id = TypesInner.tag_id [@@deriving show]

                  val unsafe_local_of_string : string -> local_user_id
                  val unsafe_remote_of_string : string -> remote_user_id
                  val unsafe_actor_of_string : string -> actor_id
                  val unsafe_remote_instance_of_string : string -> remote_instance_id
                  val unsafe_post_of_string : string -> post_id
                  val unsafe_like_of_string : string -> like_id
                  val unsafe_reboost_of_string : string -> reboost_id
                  val unsafe_follow_of_string : string -> follow_id
                  val unsafe_tag_of_string : string -> tag_id
                end)

module Activity = struct

  type t = {
    id: string;
    raw_data: Yojson.Safe.t
  } [@@deriving show]

  let decode ((id: string), (raw_data, ())) = {
    id= id;
    raw_data=Yojson.Safe.from_string raw_data
  }

  let create ~id ~data conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.insert ~table:Activity.table
      ~values:Expr.[
          Activity.id := s id;
          Activity.raw_data := s (Yojson.Safe.to_string data)
        ]
    |> Query.on_conflict `DO_NOTHING
    |> Request.make_zero
    |> Petrol.exec conn

  let find_by_id ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:Activity.table
      ~set:Expr.[
          Activity.raw_data := s (Yojson.Safe.to_string raw_data)
        ]
    |> Query.where Expr.(Activity.id = s id)
    |> Request.make_zero
    |> Petrol.exec conn

end

module Actor = struct

  type t = {
    actor_id: Types.actor_id;
    link_id: [`Local of Types.local_user_id | `Remote of Types.remote_user_id]
  }
  [@@deriving show]

  open (struct
    let lookup_local_user ~(id: Types.local_user_id) conn =
      let open Lwt_result.Syntax in
      let open Petrol in
      let open Petrol.Postgres in
      let open Tables in
      Query.select [Actor.id]
        ~from:Actor.table
      |> Query.where Expr.(Actor.local_id = vl ~ty:Type.big_serial (id :> int64))
      |> Request.make_one
      |> Petrol.find conn
      |> Lwt_result.map (fun (id, ()) -> TypesInner.actor_of_int64 id)

    let lookup_remote_user ~(id: Types.remote_user_id) conn =
      let open Lwt_result.Syntax in
      let open Petrol in
      let open Petrol.Postgres in
      let open Tables in
      Query.select [Actor.id]
        ~from:Actor.table
      |> Query.where Expr.(Actor.remote_id = vl ~ty:Type.big_serial (id :> int64))
      |> Request.make_one
      |> Petrol.find conn
      |> Lwt_result.map (fun (id, ()) -> TypesInner.actor_of_int64 id)
  end)

  let resolve ~(id: Types.actor_id) conn : ([ `Local of Types.local_user_id | `Remote of Types.remote_user_id ], _) Lwt_result.t =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[
        nullable Actor.local_id;
        nullable Actor.remote_id
      ] ~from:Actor.table
    |> Query.where Expr.(Actor.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (function
        | (Some local_id, (None, ())) -> `Local (TypesInner.local_of_int64 local_id)
        | (None, (Some remote_id, ())) -> `Remote (TypesInner.remote_of_int64 remote_id)
        | (Some _, (Some _, ())) ->
          invalid_arg "found actor id resolving to both local and remote"
        | (None, (None, ())) ->
          invalid_arg "found actor id resolving to neither local or remote"
      )

  let create_local_user ~(local_id: Types.local_user_id) conn : (Types.actor_id, _) Lwt_result.t =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:Actor.table
        ~values:Expr.[Actor.local_id := vl ~ty:Type.big_serial (local_id:> int64)]
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    lookup_local_user ~id:(local_id) conn

  let create_remote_user ~(remote_id: Types.remote_user_id) conn  : (Types.actor_id, _) Lwt_result.t =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:Actor.table
        ~values:Expr.[Actor.remote_id := vl ~ty:Type.big_serial (remote_id :> int64)]
      |> Query.on_conflict `DO_NOTHING
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
    id: Types.local_user_id;
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
      id=TypesInner.local_of_int64 id;
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
    let open Petrol.Postgres in
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

  let promote_user_to_admin ~username conn =
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[LocalUser.is_admin := true_]
    |> Query.where Expr.(LocalUser.username = s username)
    |> Request.make_zero
    |> Petrol.exec conn

  let resolve ~(id: Types.local_user_id) conn =
    let open Petrol in
    let open Petrol.Postgres in
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
    |> Query.where Expr.(LocalUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create_user ?display_name ?about ?(manually_accept_follows=false) ?(is_admin=false) ~username ~password conn =
    let create_actor local_id conn = Actor.create_local_user ~local_id conn in
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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

  let update_password ~(id: Types.local_user_id) ~password conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* password_hash =
      Lwt.return @@ (Password.hash ~pwd:password
                     |> Result.map_error (fun err -> `ArgonError err)) in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
          LocalUser.password := s password_hash
        ]
    |> Query.where Expr.(LocalUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let update_display_name ~(id: Types.local_user_id) ~display_name conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
          LocalUser.display_name := s display_name
        ]
    |> Query.where Expr.(LocalUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let update_about ~(id: Types.local_user_id) ~about conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
          LocalUser.about := s about
        ]
    |> Query.where Expr.(LocalUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let update_manually_accept_follows ~(id: Types.local_user_id) ~manually_accept_follows conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
          LocalUser.manually_accept_follows := bl manually_accept_follows 
        ]
    |> Query.where Expr.(LocalUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn  

  let update_is_admin ~(id: Types.local_user_id) ~is_admin conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
          LocalUser.is_admin := bl is_admin
        ]
    |> Query.where Expr.(LocalUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let update_profile_picture ~(id: Types.local_user_id) ~image conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:LocalUser.table
      ~set:Expr.[
          LocalUser.profile_picture := s image
        ]
    |> Query.where Expr.(LocalUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let collect_local_users ?(offset=0) ?(limit=10) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[count_star] ~from:LocalUser.table
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let find_local_users ?(offset=0) ?(limit=10) ~pattern conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let query =
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
      |> Query.offset Expr.(i offset) in
    query
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let find_local_user_count ~pattern conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[count_star]
      ~from:LocalUser.table
    |> Query.where Expr.(like LocalUser.username ~pat:(s pattern) ||
                         like LocalUser.display_name ~pat:(s pattern))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

end

module RemoteInstance = struct

  type t = {
    id: Types.remote_instance_id;
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
      id= TypesInner.remote_instance_of_int64 id;
      url;
      last_unreachable
    }

  let lookup_instance ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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

  let update_instance_last_unreachable ~(id: Types.remote_instance_id) ~last_unreachable conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:RemoteInstance.table
      ~set:Expr.[
          RemoteInstance.last_unreachable :=
            s (Ptime.to_rfc3339 last_unreachable)
        ]
    |> Query.where
      Expr.(RemoteInstance.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let unset_instance_last_unreachable ~(id: Types.remote_instance_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.update ~table:RemoteInstance.table
      ~set:Expr.[
          unset RemoteInstance.last_unreachable
        ]
    |> Query.where
      Expr.(RemoteInstance.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let resolve ~(id: Types.remote_instance_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[
        RemoteInstance.id;
        RemoteInstance.url;
        nullable RemoteInstance.last_unreachable
      ] ~from:RemoteInstance.table
    |> Query.where
      Expr.(RemoteInstance.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create_instance ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () = 
      Query.insert ~table:RemoteInstance.table
        ~values:Expr.[
            RemoteInstance.url := s url
          ]
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    let* instance = lookup_instance ~url conn in
    Lwt_result.return (Option.get instance)

  let find_possible_remote_instances_to_query
      ?(offset=0) ?(limit=10) pattern conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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

  let resolve_by_hash ~hash conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select
      Expr.[UserImage.path; UserImage.hash]
      ~from:UserImage.table
    |> Query.where Expr.(UserImage.hash = vl ~ty:Petrol.Postgres.Type.bytea hash)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let find_by_path ~path conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    let open Petrol.Postgres in
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
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:UserImage.table
        ~values:Expr.[
            UserImage.path := s path;
            UserImage.hash := vl ~ty:Petrol.Postgres.Type.bytea hash;
          ]
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    find_by_path ~path conn

end


module RemoteUser = struct

  type t = {
    id: Types.remote_user_id;
    username: string;
    instance_id: Types.remote_instance_id;
    display_name: string option;
    url: string;
    inbox: string option;
    outbox: string option;
    followers: string option;
    following: string option;
    summary: string option;
    public_key_pem: string;
    profile_picture: string option;
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
                (public_key_pem,
                 (profile_picture, ())))))))))))) = {
    id=TypesInner.remote_of_int64 id;
    username;
    instance_id = TypesInner.remote_instance_of_int64 instance_id;
    display_name;
    url;
    inbox;
    outbox;
    followers;
    following;
    summary;
    public_key_pem;
    profile_picture
  }

  let lookup_remote_user_by_address ~username ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        RemoteUser.public_key_pem;
        nullable RemoteUser.profile_picture;
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
    let open Petrol.Postgres in
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
        RemoteUser.public_key_pem;
        nullable RemoteUser.profile_picture;
      ]
      ~from:RemoteUser.table
    |> Query.where Expr.(RemoteUser.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let resolve ~(id: Types.remote_user_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        RemoteUser.public_key_pem;
        nullable RemoteUser.profile_picture;
      ]
      ~from:RemoteUser.table
    |> Query.where Expr.(RemoteUser.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create_remote_user
      ?display_name ?inbox ?outbox ?followers ?following ?summary ?profile_picture
      ~username ~(instance:Types.remote_instance_id) ~url ~public_key_pem conn =
    let create_remote remote_id conn = Actor.create_remote_user ~remote_id conn in
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:RemoteUser.table
        ~values:(
          Expr.[
            RemoteUser.username := s username;
            RemoteUser.instance_id := vl ~ty:Type.big_serial (instance :> int64);
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
          Expr.[RemoteUser.public_key_pem := s public_key_pem] @
          (profile_picture |> Option.map Expr.(fun n ->
               RemoteUser.profile_picture := s n
             ) |> Option.to_list)
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
    let open Petrol.Postgres in
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
    let open Petrol.Postgres in
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
        RemoteUser.public_key_pem;
        nullable RemoteUser.profile_picture;
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
      ?(offset=0) ?(limit=10) ~target:(target_id: Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        RemoteUser.public_key_pem;
        nullable RemoteUser.profile_picture;
      ] ~from:RemoteUser.table
    |> Query.join ~op:INNER ~on:Expr.(a_rid_ref = RemoteUser.id)
      (Query.select [a_id; a_rid] ~from:Actor.table)
    |> Query.where
      (Expr.exists begin
          Query.select [Follows.id;Follows.author_id;Follows.target_id]
            ~from:Follows.table
          |> Query.where Expr.(
              Follows.author_id = a_id_ref &&
              Follows.target_id = vl ~ty:Type.big_serial (target_id :> int64) &&
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
    let open Petrol.Postgres in
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
        RemoteUser.public_key_pem;
        nullable RemoteUser.profile_picture;
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
    id: Types.tag_id;
    name: string
  }
  [@@deriving show]

  let decode (id, (name, ())) =
    { id= TypesInner.tag_of_int64 id; name }

  let find_by_name ~tag conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select [Tag.id; Tag.name] ~from:Tag.table
    |> Query.where Expr.(Tag.name = s tag)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let resolve ~(id: Types.tag_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select [Tag.id; Tag.name] ~from:Tag.table
    |> Query.where Expr.(Tag.id = vl ~ty:Type.big_serial (id:> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create ~name conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:Tag.table
        ~values:Expr.[Tag.name := s name]
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    find_by_name ~tag:name conn

end

module Posts = struct

  type t = {
    id: Types.post_id;
    public_id: string option;
    url: string;
    author_id: Types.actor_id;

    in_reply_to: string option;

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

  let decode (id,
              (public_id,
               (url,
                (author_id,
                 (is_public,
                  (is_follower_public,
                   (summary,
                    (content_type,
                     (post_source,
                      (published,
                       (raw_data,
                        (in_reply_to, ())))))))))))) =
    let published =
      Ptime.of_rfc3339 published
      |> Result.get_ok
      |> (fun (t, _, _) -> t) in
    let raw_data = Option.map Yojson.Safe.from_string raw_data in
    {
      id = TypesInner.post_of_int64 id;
      public_id;
      url;
      author_id = TypesInner.actor_of_int64 author_id;
      is_public; is_follower_public;
      summary;
      content_type=int_to_content_type content_type;
      post_source;
      published;
      raw_data;
      in_reply_to;
    }

  let lookup_by_public_id ~public_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.where Expr.(Posts.public_id = s public_id)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let lookup_by_url ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.where Expr.(Posts.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let delete ~(id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    (* when we delete a post, we just set all it's data + metadata to null *)
    Query.update ~table:Posts.table ~set:Expr.[
        unset Posts.summary;
        Posts.post_source := s "";
        Posts.raw_data := s "null";
        Posts.published := s (Ptime.to_rfc3339 Ptime.epoch);
        Posts.deleted := true_;
      ]
    |> Query.where Expr.(Posts.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let resolve ~(id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.where Expr.(Posts.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create ?public_id ?summary ?raw_data ?in_reply_to
      ?(is_public=true) ?(is_follower_public=true)
      ~url ~(author: Types.actor_id) ~post_content
      ~post_source ~published conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:Posts.table
        ~values:(
          (public_id
           |> Option.map Expr.(fun t -> Posts.public_id := s t)
           |> Option.to_list) @
          Expr.[
            Posts.url := s url;
            Posts.author_id := vl ~ty:Type.big_serial (author :> int64);
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
           |> Option.to_list) @
          (in_reply_to
           |> Option.map Expr.(fun url ->
               Posts.in_reply_to := s url)
           |> Option.to_list)
        ) 
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    let* data = lookup_by_url ~url conn in
    Lwt_result.return (Option.get data)

  let count_posts_by_author ~(author:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[count_star]
      ~from:Posts.table
    |> Query.where Expr.(Posts.author_id = vl ~ty:Type.big_serial (author :> int64) && ((not Posts.deleted) || is_null Posts.deleted))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (id, ()) -> id)

  let collect_posts_by_author
      ?(offset=0) ?(limit=10) ~start_time ~(author: Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.where
      Expr.(Posts.author_id = vl ~ty:Type.big_serial (author :> int64) &&
            Posts.published <= s start_time &&
            Posts.is_public = true_ && ((not Posts.deleted) || is_null Posts.deleted))
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Query.order_by ~direction:`DESC Posts.published
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let post_to ?(offset=0) ?(limit=10) ~(id: Types.post_id) conn : (Types.actor_id list, _) Lwt_result.t =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select [Posts.PostTo.actor_id]
      ~from:Posts.PostTo.table
    |> Query.where Expr.(Posts.PostTo.post_id = vl ~ty:Type.big_serial (id :> int64))
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> TypesInner.actor_of_int64 id))

  let post_cc ?(offset=0) ?(limit=10) ~(id: Types.post_id) conn : (Types.actor_id list, _) Lwt_result.t =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select [Posts.PostCc.actor_id]
      ~from:Posts.PostCc.table
    |> Query.where Expr.(Posts.PostCc.post_id = vl ~ty:Type.big_serial (id :> int64))
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> TypesInner.actor_of_int64 id))

  let post_mentions ?(offset=0) ?(limit=10) ~(id: Types.post_id) conn : (Types.actor_id list, _) Lwt_result.t =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select [Posts.PostMentions.actor_id]
      ~from:Posts.PostMentions.table
    |> Query.where Expr.(Posts.PostMentions.post_id = vl ~ty:Type.big_serial (id :> int64))
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> TypesInner.actor_of_int64 id))

  let post_tags ?(offset=0) ?(limit=10) ~(id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let tag_name, tag_name_ref = Expr.as_ Tag.name ~name:"t_name" in
    let tag_id, tag_id_ref = Expr.as_ Tag.id ~name:"t_id" in
    Query.select [tag_name_ref]
      ~from:Posts.PostTags.table
    |> Query.join ~op:INNER (
      Query.select Expr.[tag_id; tag_name] ~from:Tag.table
    ) ~on:Expr.(tag_id_ref = Posts.PostTags.tag_id)
    |> Query.where Expr.(Posts.PostTags.post_id = vl ~ty:Type.big_serial (id :> int64))
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (id, ()) -> id))

  let add_post_to ~(id: Types.post_id) ~(actor_id: Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.insert ~table:Posts.PostTo.table
      ~values:Expr.[
          Posts.PostTo.post_id := vl ~ty:Type.big_serial (id :> int64);
          Posts.PostTo.actor_id := vl ~ty:Type.big_serial (actor_id :> int64);
        ]
    |> Query.on_conflict `DO_NOTHING
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_tos ~(id: Types.post_id) ~tos conn =
    Lwt_list.fold_left_s (fun acc (actor_id: Types.actor_id) -> match acc with
        | Ok () ->
          add_post_to ~id ~actor_id conn
        | Error _ -> Lwt.return acc
      ) (Ok ()) tos

  let add_post_cc ~(id: Types.post_id) ~(actor_id: Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.insert ~table:Posts.PostCc.table
      ~values:Expr.[
          Posts.PostCc.post_id := vl ~ty:Type.big_serial (id :> int64);
          Posts.PostCc.actor_id := vl ~ty:Type.big_serial (actor_id :> int64);
        ]
    |> Query.on_conflict `DO_NOTHING
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_ccs ~id ~ccs conn =
    Lwt_list.fold_left_s (fun acc actor_id -> match acc with
        | Ok () ->
          add_post_cc ~id ~actor_id conn
        | Error _ -> Lwt.return acc
      ) (Ok ()) ccs

  let add_post_mention ~(id: Types.post_id) ~(actor_id: Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.insert ~table:Posts.PostMentions.table
      ~values:Expr.[
          Posts.PostMentions.post_id := vl ~ty:Type.big_serial (id :> int64);
          Posts.PostMentions.actor_id := vl ~ty:Type.big_serial (actor_id :> int64);
        ]
    |> Query.on_conflict `DO_NOTHING
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_mentions ~id ~mentions conn =
    Lwt_list.fold_left_s (fun acc actor_id -> match acc with
        | Ok () ->
          add_post_mention ~id ~actor_id conn
        | Error _ -> Lwt.return acc
      ) (Ok ()) mentions

  let add_post_tag ~(id: Types.post_id) ~tag conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let* tag = Tag.create ~name:tag conn in
    let open Tables in
    Query.insert ~table:Posts.PostTags.table
      ~values:Expr.[
          Posts.PostTags.post_id := vl ~ty:Type.big_serial (id :> int64);
          Posts.PostTags.tag_id := vl ~ty:Type.big_serial (tag.id :> int64);
        ]
    |> Query.on_conflict `DO_NOTHING
    |> Request.make_zero
    |> Petrol.exec conn

  let add_post_tags ~id ~tags conn =
    Lwt_list.fold_left_s (fun acc tag -> match acc with
        | Ok () ->
          add_post_tag ~id ~tag conn
        | Error _ -> Lwt.return acc
      ) (Ok ()) tags

  (* a post is a direct message to actor(id) iff *)
  let is_direct_message ~(id: Types.actor_id) =
    let open Petrol in
    let open Petrol.Postgres in
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
            Posts.PostTo.actor_id = vl ~ty:Type.big_serial (id :> int64))
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
            Posts.PostCc.actor_id = vl ~ty:Type.big_serial (id :> int64))
      end ||
      (* or, if the post is not public and not follower public, but we are the author *)
      (not Posts.is_public && not Posts.is_follower_public &&
       Posts.author_id = vl ~ty:Type.big_serial (id :> int64))
    ) 

  let is_within_time ?end_time ~start_time () =
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    match end_time with
    | None ->
      Expr.(Posts.published <= s start_time)
    | Some end_time ->
      Expr.(Posts.published <= s start_time && s end_time <= Posts.published)

  let is_reboosted_by_following ~(id: Types.actor_id) =
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let reboost_post,reboost_post_ref =
      Expr.as_ Reboosts.post_id ~name:"reboost_post_id" in
    let reboost_actor,reboost_actor_ref =
      Expr.as_ Reboosts.actor_id ~name:"reboost_actor_id" in
    let follow_author,follow_author_ref =
      Expr.as_ Follows.author_id ~name:"follows_author_id" in
    let follow_target,follow_target_ref =
      Expr.as_ Follows.target_id ~name:"follows_target_id" in
    let follow_pending,follow_pending_ref =
      Expr.as_ Follows.pending ~name:"follows_pending" in
    Expr.(
      Expr.exists begin
        Query.select Expr.[reboost_post; reboost_actor; follow_author_ref;
                           follow_pending_ref]
          ~from:Reboosts.table
        |> Query.join ~on:Expr.(follow_target_ref = reboost_actor_ref)
          ~op:Query.INNER begin
          Query.select Expr.[follow_author; follow_target; follow_pending]
            ~from:Follows.table
        end
        |> Query.where Expr.(follow_author_ref = vl ~ty:Type.big_serial (id :> int64) &&
                             reboost_post_ref = Posts.id &&
                             not follow_pending_ref)
        |> Query.order_by reboost_post_ref
      end && Posts.is_public
    )

  let is_feed_post ~(id: Types.actor_id) =
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Expr.(
      (
        (* TODO: We are not blocking/muting the author *)
        (* (1) we are the author *)
        Posts.author_id = vl ~ty:Type.big_serial (id :> int64) ||
        (* (2) we are following the author of the post && it is public *)
        (Expr.exists begin
            Query.select Expr.[Follows.author_id; Follows.target_id; Follows.pending]
              ~from:Follows.table
            |> Query.where Expr.(
                Follows.author_id = vl ~ty:Type.big_serial (id :> int64) &&
                Follows.target_id = Posts.author_id &&
                not Follows.pending
              )
          end && (Posts.is_public || Posts.is_follower_public)
        ) ||
        (* (3) it is a direct message to us *)
        is_direct_message ~id ||
        (* (4) the post has been reboosted by someone we follow & is public *)
        is_reboosted_by_following ~id
      )
    )

  let is_local_post =  
    let open Petrol in
    let open Petrol.Postgres in
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
    let open Petrol.Postgres in
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
        nullable Posts.raw_data;
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.where Expr.(is_within_time ~start_time () && is_feed_post ~id && ((not Posts.deleted) || is_null Posts.deleted))
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_feed ?start_time ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    (* collect posts where *)
    Query.select Expr.[ count_star ] ~from:Posts.table
    |> Query.where Expr.(is_within_time ~start_time () && is_feed_post ~id && ((not Posts.deleted) || is_null Posts.deleted))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let collect_direct ?(offset=0) ?(limit=10) ?start_time ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        nullable Posts.raw_data;
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.where
      Expr.(is_within_time ~start_time () && is_direct_message ~id && ((not Posts.deleted) || is_null Posts.deleted))
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_direct ~id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    (* collect posts where *)
    Query.select Expr.[ count_star ] ~from:Posts.table
    |> Query.where  Expr.(is_direct_message ~id && ((not Posts.deleted) || is_null Posts.deleted))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let collect_twkn ?(offset=0) ?(limit=10) ?start_time conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        nullable Posts.raw_data;
        nullable Posts.in_reply_to;        
      ] ~from:Posts.table
    |> Query.where
      Expr.(is_within_time ~start_time () &&
            Posts.is_public && ((not Posts.deleted) || is_null Posts.deleted))
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_twkn conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    (* collect posts where *)
    Query.select Expr.[count_star] ~from:Posts.table
    |> Query.where
      Expr.(Posts.is_public && ((not Posts.deleted) || is_null Posts.deleted))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let collect_local ?(offset=0) ?(limit=10) ?start_time conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        nullable Posts.raw_data;
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.where Expr.(is_within_time ~start_time () &&
                         Posts.is_public &&
                         is_local_post && ((not Posts.deleted) || is_null Posts.deleted))
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_local conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    (* collect posts where *)
    Query.select Expr.[count_star] ~from:Posts.table
    |> Query.where
      Expr.(Posts.is_public && is_local_post && ((not Posts.deleted) || is_null Posts.deleted))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, ()) -> count)

  let is_visible_post ~by:author_id ~post:(post_id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[ count_star ] ~from:Posts.table
    |> Query.where Expr.(Posts.id = vl ~ty:Type.big_serial (post_id :> int64) &&
                         is_feed_post ~id:author_id && ((not Posts.deleted) || is_null Posts.deleted))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (post_count, ()) -> post_count > 0)

  let count_related_posts ?start_time ~user:(user_id: Types.actor_id) ~post:(post_id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    let post_context_parent, post_context_parent_ref = Expr.as_ ~name:"post_context_parent" Posts.PostContext.parent in
    let post_context_child, post_context_child_ref = Expr.as_ ~name:"post_context_child" Posts.PostContext.child in
    (* collect posts where *)
    Query.select
      Expr.[
        count_star;
      ] ~from:Posts.table
    |> Query.join ~op:Query.INNER begin
      Query.select Expr.[
          post_context_parent;
          post_context_child
        ] ~from:Posts.PostContext.table
    end ~on:Expr.(post_context_child_ref = Posts.id)
    |> Query.where Expr.(is_within_time ~start_time () &&
                         (is_feed_post ~id:user_id || Posts.is_public) &&
                         ((not Posts.deleted) || is_null Posts.deleted) &&
                         post_context_parent_ref = vl ~ty:Type.big_serial (post_id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (count, _) -> count)

  let collect_related_posts ?(offset=0) ?(limit=10) ?start_time ~user:user_id ~post:(post_id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let start_time =
      Option.value start_time
        ~default:(Ptime_clock.now ())
      |> Ptime.to_rfc3339 in
    let post_context_parent, post_context_parent_ref = Expr.as_ ~name:"post_context_parent" Posts.PostContext.parent in
    let post_context_child, post_context_child_ref = Expr.as_ ~name:"post_context_child" Posts.PostContext.child in
    (* collect posts where *)
    Query.select
      Expr.[
        post_context_parent_ref;
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
        nullable Posts.in_reply_to;
      ] ~from:Posts.table
    |> Query.join ~op:Query.INNER begin
      Query.select Expr.[
          post_context_parent;
          post_context_child
        ] ~from:Posts.PostContext.table
    end ~on:Expr.(post_context_child_ref = Posts.id)
    |> Query.where Expr.(is_within_time ~start_time () &&
                         (is_feed_post ~id:user_id || Posts.is_public) &&
                         ((not Posts.deleted) || is_null Posts.deleted) &&
                         post_context_parent_ref = vl ~ty:Type.big_serial (post_id :> int64))
    |> Query.order_by Posts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (_, post) -> decode post))

  open (struct
    let collect_child_post_ids ~post:(post_id: Types.post_id) conn =
      let open Lwt_result.Syntax in
      let open Petrol in
      let open Petrol.Postgres in
      let open Tables in
      let post_context_parent, post_context_parent_ref = Expr.as_ ~name:"post_context_parent" Posts.PostContext.parent in
      let post_context_child, post_context_child_ref = Expr.as_ ~name:"post_context_child" Posts.PostContext.child in
      (* collect posts where *)
      Query.select Expr.[ Posts.id; ] ~from:Posts.table
      |> Query.join ~op:Query.INNER begin
        Query.select Expr.[
            post_context_parent;
            post_context_child
          ] ~from:Posts.PostContext.table
      end ~on:Expr.(post_context_child_ref = Posts.id)
      |> Query.where Expr.(((not Posts.deleted) || is_null Posts.deleted) &&
                           post_context_parent_ref = vl ~ty:Type.big_serial (post_id :> int64))
      |> Query.order_by Posts.published ~direction:`DESC
      |> Request.make_many
      |> Petrol.collect_list conn
      |> Lwt_result.map (List.map (fun (id, _) -> id))

    let collect_parent_post_ids ~post:(post_id: Types.post_id) conn =
      let open Lwt_result.Syntax in
      let open Petrol in
      let open Petrol.Postgres in
      let open Tables in
      let post_context_parent, post_context_parent_ref = Expr.as_ ~name:"post_context_parent" Posts.PostContext.parent in
      let post_context_child, post_context_child_ref = Expr.as_ ~name:"post_context_child" Posts.PostContext.child in
      (* collect posts where *)
      Query.select Expr.[ Posts.id; ] ~from:Posts.table
      |> Query.join ~op:Query.INNER begin
        Query.select Expr.[
            post_context_parent;
            post_context_child
          ] ~from:Posts.PostContext.table
      end ~on:Expr.(post_context_parent_ref = Posts.id)
      |> Query.where Expr.(((not Posts.deleted) || is_null Posts.deleted) &&
                           post_context_child_ref = vl ~ty:Type.big_serial (post_id :> int64))
      |> Query.order_by Posts.published ~direction:`DESC
      |> Request.make_many
      |> Petrol.collect_list conn
      |> Lwt_result.map (List.map (fun (id, _) -> TypesInner.post_of_int64 id))
  end)

  let record_reply_relation ~parent:(parent_id: Types.post_id) ~child:(child_id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.insert ~table:Posts.PostContext.table
      ~values:Expr.[Posts.PostContext.parent := vl ~ty:Type.big_serial (parent_id :> int64);
                    Posts.PostContext.child := vl ~ty:Type.big_serial (child_id :> int64)]
    |> Query.on_conflict `DO_NOTHING
    |> Request.make_zero
    |> Petrol.exec conn

  let record_reply_relation ~parent:parent_id ~child:child_id conn =
    let open Lwt_result.Syntax in
    let* _ = record_reply_relation ~parent:parent_id ~child:child_id conn in
    let* parents = collect_parent_post_ids ~post:parent_id conn in
    Lwt_list.map_s (fun parent_id ->
        record_reply_relation ~parent:parent_id ~child:child_id conn
      ) parents
    |> Lwt.map Containers.List.all_ok
    |> Lwt_result.map ignore

  let add_attachment ~post:(post_id: Types.post_id) ?media_type ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.insert
      ~table:Tables.Posts.PostAttachments.table
      ~values:Expr.([
          Tables.Posts.PostAttachments.post_id := vl ~ty:Type.big_serial (post_id :> int64);
          Tables.Posts.PostAttachments.url := s url;
        ] @
          (Option.map (fun vl -> Tables.Posts.PostAttachments.media_type := s vl) media_type |> Option.to_list)
        )
    |> Request.make_zero
    |> Petrol.exec conn

  let collect_attachments ~post:(post_id: Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in

    Query.select Expr.[
        nullable Tables.Posts.PostAttachments.media_type;
        Tables.Posts.PostAttachments.url;
      ] ~from:Tables.Posts.PostAttachments.table
    |> Query.where Expr.(Tables.Posts.PostAttachments.post_id = vl ~ty:Type.big_serial (post_id :> int64))
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map (fun (media_type, (url, ())) -> (media_type, url)))

end

module Follows = struct

  type t = {
    id: Types.follow_id;
    public_id: string option;
    url: string;
    raw_data: Yojson.Safe.t option;
    pending: bool;
    created: Ptime.t;
    updated: Ptime.t option;
    author_id: Types.actor_id;
    target_id: Types.actor_id;
  } [@@deriving show]

  let decode (id, (public_id, (url, (raw_data, (pending, (created, (updated, (author_id, (target_id, ()))))))))) =
    let raw_data = Option.map Yojson.Safe.from_string raw_data in
    let from_string s = Ptime.of_rfc3339 s |> Result.get_ok |> (fun (time, _, _) -> time) in
    let created = from_string created in
    let updated = Option.map from_string updated in
    {
      id=TypesInner.follow_of_int64 id;
      public_id;
      url;
      raw_data;
      pending;
      created;
      updated;
      author_id = TypesInner.actor_of_int64 author_id;
      target_id = TypesInner.actor_of_int64 target_id;
    }

  let lookup_by_public_id ~public_id conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    let open Petrol.Postgres in
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

  let resolve ~(id: Types.follow_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    |> Query.where Expr.(Follows.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let create ?public_id ?raw_data ?updated ~url ~(author: Types.actor_id) ~(target:Types.actor_id) ~pending ~created conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:Follows.table ~values:(
        Expr.[
          Follows.url := s url;
          Follows.pending := bl pending;
          Follows.created := s (Ptime.to_rfc3339 created);
          Follows.author_id := vl ~ty:Type.big_serial (author :> int64);
          Follows.target_id := vl ~ty:Type.big_serial (target :> int64);
        ] 
        @ (Option.map Expr.(fun public_id -> Follows.public_id := s public_id) public_id |> Option.to_list)
        @ (Option.map Expr.(fun raw_data -> Follows.raw_data := s (Yojson.Safe.to_string raw_data)) raw_data |> Option.to_list)
        @ (Option.map Expr.(fun updated -> Follows.updated := s (Ptime.to_rfc3339 updated)) updated |> Option.to_list)
      )
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    let* result = lookup_by_url ~url conn in
    Lwt_result.return (Option.get result)

  let update_pending_status ~(id: Types.follow_id) ~pending conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let updated = Ptime_clock.now () in
    Query.update ~table:Follows.table
      ~set:Expr.[
          Follows.pending := bl pending;
          Follows.updated := s (Ptime.to_rfc3339 updated);
        ]
    |> Query.where Expr.(Follows.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let delete ~(id: Types.follow_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.delete ~from:Follows.table
    |> Query.where Expr.(Follows.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let collect_follows_for_actor ?(offset=0) ?(limit=10) ?since ~(id: Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        (Follows.target_id = vl ~ty:Type.big_serial (id :> int64) ||
         Follows.author_id = vl ~ty:Type.big_serial (id :> int64)) &&
        (coalesce [Follows.updated; Follows.created]) <= s since &&
        Follows.pending = true_
      )
    |> Query.order_by Expr.(coalesce [Follows.updated; Follows.created]) ~direction:`DESC
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let is_following ~(author:Types.actor_id) ~(target:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[count_star] ~from:Follows.table
    |> Query.where Expr.(Follows.author_id = vl ~ty:Type.big_serial (author :> int64) &&
                         Follows.target_id = vl ~ty:Type.big_serial (target :> int64) &&
                         Follows.pending = false_)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (c, ()) -> c > 0)

  let find_follow_between ~(author:Types.actor_id) ~(target:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    |> Query.where Expr.(Follows.author_id = vl ~ty:Type.big_serial (author :> int64) &&
                         Follows.target_id = vl ~ty:Type.big_serial (target :> int64))
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let count_following ~(author:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[count_star] ~from:Follows.table
    |> Query.where Expr.(Follows.author_id = vl ~ty:Type.big_serial (author :> int64) &&
                         Follows.pending = false_)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (c, ()) -> c)

  let collect_following_for_actor ?(offset=0) ?(limit=10) ?since ~(id:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        (Follows.author_id = vl ~ty:Type.big_serial (id :> int64)) &&
        (coalesce [Follows.updated; Follows.created]) <= s since &&
        Follows.pending = false_
      )
    |> Query.order_by Expr.(coalesce [Follows.updated; Follows.created]) ~direction:`DESC
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let count_followers ~(target:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[count_star] ~from:Follows.table
    |> Query.where Expr.(Follows.target_id = vl ~ty:Type.big_serial (target :> int64) &&
                         Follows.pending = false_)
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (c, ()) -> c)

  let collect_followers_for_actor ?(offset=0) ?(limit=10) ?since ~(id:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
        (Follows.target_id = vl ~ty:Type.big_serial (id :> int64)) &&
        (coalesce [Follows.updated; Follows.created]) <= s since &&
        Follows.pending = false_
      )
    |> Query.order_by Expr.(coalesce [Follows.updated; Follows.created]) ~direction:`DESC
    |> Query.offset Expr.(i offset)
    |> Query.limit Expr.(i limit)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let target req conn =
    let open Lwt_result.Syntax in
    let* target = Actor.resolve ~id:req.target_id conn in
    match target with
    | `Local id ->
      let+ user = LocalUser.resolve ~id conn in
      `Local user
    | `Remote id ->
      let+ user = RemoteUser.resolve ~id conn in
      `Remote user

  let author req conn =
    let open Lwt_result.Syntax in
    let* target = Actor.resolve ~id:req.author_id conn in
    match target with
    | `Local id ->
      let+ user = LocalUser.resolve ~id conn in
      `Local user
    | `Remote id ->
      let+ user = RemoteUser.resolve ~id conn in
      `Remote user

end

module Reboosts = struct

  type t = {
    id: Types.reboost_id;
    public_id: string option;
    url: string;
    raw_data: Yojson.Safe.t option;
    published: Ptime.t;
    post_id: Types.post_id;
    actor_id: Types.actor_id;
  }
  [@@deriving show]

  let decode (id, (public_id, (url, (raw_data, (published, (post_id, (actor_id, ()))))))) =
    let raw_data = Option.map Yojson.Safe.from_string raw_data in
    let from_string s = Ptime.of_rfc3339 s |> Result.get_ok |> (fun (time, _, _) -> time) in
    let published = from_string published in
    {
      id = TypesInner.reboost_of_int64 id;
      public_id;
      url;
      raw_data;
      published;
      post_id=TypesInner.post_of_int64 post_id;
      actor_id=TypesInner.actor_of_int64 actor_id;
    }

  let delete ~(id: Types.reboost_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.delete ~from:Reboosts.table
    |> Query.where Expr.(Reboosts.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let resolve ~(id: Types.reboost_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[
        Reboosts.id;
        nullable Reboosts.public_id;
        Reboosts.url;
        nullable Reboosts.raw_data;
        Reboosts.published;
        Reboosts.post_id;
        Reboosts.actor_id;
      ] ~from:Reboosts.table
    |> Query.where Expr.(Reboosts.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let lookup_by_url ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[
        Reboosts.id;
        nullable Reboosts.public_id;
        Reboosts.url;
        nullable Reboosts.raw_data;
        Reboosts.published;
        Reboosts.post_id;
        Reboosts.actor_id;
      ] ~from:Reboosts.table
    |> Query.where Expr.(Reboosts.url = s url)
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let create ?public_id ?raw_data ~url ~(post:Types.post_id) ~(actor:Types.actor_id) ~published conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:Reboosts.table ~values:(
        Expr.[
          Reboosts.url := s url;
          Reboosts.published := s (Ptime.to_rfc3339 published);
          Reboosts.post_id := vl ~ty:Type.big_serial (post :> int64);
          Reboosts.actor_id := vl ~ty:Type.big_serial (actor :> int64);
        ] 
        @ (Option.map Expr.(fun public_id ->
            Reboosts.public_id := s public_id) public_id
           |> Option.to_list)
        @ (Option.map Expr.(fun raw_data ->
            Reboosts.raw_data := s (Yojson.Safe.to_string raw_data)) raw_data
           |> Option.to_list)
      )
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    let* result = lookup_by_url ~url conn in
    Lwt_result.return (Option.get result)

  let find_reboost_between ~(post:Types.post_id) ~(author:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[
        Reboosts.id;
        nullable Reboosts.public_id;
        Reboosts.url;
        nullable Reboosts.raw_data;
        Reboosts.published;
        Reboosts.post_id;
        Reboosts.actor_id;
      ] ~from:Reboosts.table
    |> Query.where Expr.(Reboosts.post_id = vl ~ty:Type.big_serial (post :> int64) &&
                         Reboosts.actor_id = vl ~ty:Type.big_serial (author :> int64))
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let count_for_post ~(post:Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[ count_star ] ~from:Reboosts.table
    |> Query.where Expr.(Reboosts.post_id = vl ~ty:Type.big_serial (post :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (id, ()) -> id)

  let collect_for_post ?(offset=0) ?(limit=10) ~(post:Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[
        Reboosts.id;
        nullable Reboosts.public_id;
        Reboosts.url;
        nullable Reboosts.raw_data;
        Reboosts.published;
        Reboosts.post_id;
        Reboosts.actor_id;
      ] ~from:Reboosts.table
    |> Query.where Expr.(Reboosts.post_id = vl ~ty:Type.big_serial (post :> int64))
    |> Query.order_by Reboosts.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let collect_relevant_for_user ~(post:Types.post_id) ~(user:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[
        Reboosts.id;
        nullable Reboosts.public_id;
        Reboosts.url;
        nullable Reboosts.raw_data;
        Reboosts.published;
        Reboosts.post_id;
        Reboosts.actor_id;
      ] ~from:Reboosts.table
    |> Query.where Expr.(
        Reboosts.post_id = vl ~ty:Type.big_serial (post :> int64) &&
        Expr.exists begin
          Query.select Expr.[Follows.author_id; Follows.target_id]
            ~from:Follows.table
          |> Query.where Expr.(
              Follows.target_id = Reboosts.actor_id &&
              Follows.author_id = vl ~ty:Type.big_serial (user :> int64) &&
              not Follows.pending
            )
        end
      )
    |> Query.order_by Reboosts.published ~direction:`DESC
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

end

module Likes = struct

  type t = {
    id: Types.like_id;
    public_id: string option;
    url: string;
    raw_data: Yojson.Safe.t option;
    published: Ptime.t;
    post_id: Types.post_id;
    actor_id: Types.actor_id;
  }
  [@@deriving show]

  let decode (id, (public_id, (url, (raw_data, (published, (post_id, (actor_id, ()))))))) =
    let raw_data = Option.map Yojson.Safe.from_string raw_data in
    let from_string s = Ptime.of_rfc3339 s |> Result.get_ok |> (fun (time, _, _) -> time) in
    let published = from_string published in
    {
      id=TypesInner.like_of_int64 id;
      public_id;
      url;
      raw_data;
      published;
      post_id=TypesInner.post_of_int64 post_id;
      actor_id=TypesInner.actor_of_int64 actor_id;
    }

  let resolve ~(id: Types.like_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    |> Query.where Expr.(Likes.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map decode

  let delete ~(id: Types.like_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.delete ~from:Likes.table
    |> Query.where Expr.(Likes.id = vl ~ty:Type.big_serial (id :> int64))
    |> Request.make_zero
    |> Petrol.exec conn

  let lookup_by_url ~url conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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

  let create ?public_id  ?raw_data ~url ~(post:Types.post_id) ~(actor:Types.actor_id) ~published conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* () =
      Query.insert ~table:Likes.table ~values:(
        Expr.[
          Likes.url := s url;
          Likes.published := s (Ptime.to_rfc3339 published);
          Likes.post_id := vl ~ty:Type.big_serial (post :> int64);
          Likes.actor_id := vl ~ty:Type.big_serial (actor :> int64);
        ] 
        @ (Option.map Expr.(fun public_id -> Likes.public_id := s public_id) public_id |> Option.to_list)
        @ (Option.map Expr.(fun raw_data -> Likes.raw_data := s (Yojson.Safe.to_string raw_data)) raw_data |> Option.to_list)
      )
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> Petrol.exec conn in
    let* result = lookup_by_url ~url conn in
    Lwt_result.return (Option.get result)

  let find_like_between ~(post:Types.post_id) ~(author:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    |> Query.where Expr.(Likes.post_id = vl ~ty:Type.big_serial (post :> int64) &&
                         Likes.actor_id = vl ~ty:Type.big_serial (author :> int64))
    |> Request.make_zero_or_one
    |> Petrol.find_opt conn
    |> Lwt_result.map (Option.map decode)

  let count_for_post ~(post:Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    Query.select Expr.[ count_star ] ~from:Likes.table
    |> Query.where Expr.(Likes.post_id = vl ~ty:Type.big_serial (post :> int64))
    |> Request.make_one
    |> Petrol.find conn
    |> Lwt_result.map (fun (id, ()) -> id)

  let collect_for_post ?(offset=0) ?(limit=10) ~(post:Types.post_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    |> Query.where Expr.(Likes.post_id = vl ~ty:Type.big_serial (post :> int64))
    |> Query.order_by Likes.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

  let collect_by_actor ?(offset=0) ?(limit=10) ~(actor:Types.actor_id) conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
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
    |> Query.where Expr.(Likes.actor_id = vl ~ty:Type.big_serial (actor :> int64))
    |> Query.order_by Likes.published ~direction:`DESC
    |> Query.limit Expr.(i limit)
    |> Query.offset Expr.(i offset)
    |> Request.make_many
    |> Petrol.collect_list conn
    |> Lwt_result.map (List.map decode)

end


module Admin = struct


  let is_registration_allowed conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* _ =
      Query.insert ~table:Admin.table ~values:Expr.[
          Admin.key := s_stat "is_registration_allowed";
          Admin.value := s_stat "true"
        ]
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> exec conn in
    Query.select Expr.[ Admin.value ] ~from:Admin.table
    |> Query.where Expr.(Admin.key = Expr.s_stat "is_registration_allowed")
    |> Request.make_one
    |> find conn
    |> Lwt_result.map (function[@warning "-8"] "true", _ -> true | "false", _ -> false)

  let set_registration_allowed allowed conn =
    let open Lwt_result.Syntax in
    let open Petrol in
    let open Petrol.Postgres in
    let open Tables in
    let* _ =
      Query.insert ~table:Admin.table ~values:Expr.[
          Admin.key := s_stat "is_registration_allowed";
          Admin.value := s_stat "true"
        ]
      |> Query.on_conflict `DO_NOTHING
      |> Request.make_zero
      |> exec conn in
    Query.update ~table:Admin.table ~set:Expr.[ Admin.value := s_stat (if allowed then  "true" else "false")]
    |> Query.where Expr.(Admin.key = Expr.s_stat "is_registration_allowed")
    |> Request.make_zero
    |> exec conn



end

