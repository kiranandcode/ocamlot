open Petrol

let version_0_0_1 = VersionedSchema.version [0;0;1]

let db = VersionedSchema.init version_0_0_1 ~name:"ocamlot"

module Activity = struct

  type t = {
    id: string;
    raw_data: Yojson.Safe.t
  }

  let table, Expr.[id; raw_data] =
    VersionedSchema.declare_table db ~name:"activity"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.text;                     (* uuid of the data *)
        field ~constraints:[not_null ()] "raw_data" ~ty:Type.blob;                  (* json data (Yojson.t) *)
      ]

end

module LocalUser = struct

  let table, (Expr.[id; username; password; display_name; about; manually_accept_follows; is_admin; pubkey; privkey] as all_fields) =
    VersionedSchema.declare_table db ~name:"local_user"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.int;                      (* internal id, not exposed *)
        field ~constraints:[unique (); not_null ()] "username" ~ty:Type.text;       (* username *)
        field ~constraints:[not_null ()] "password" ~ty:Type.text;                  (* password hash + salt *)
        field "display_name" ~ty:Type.text;                                         (* display name - if null then username *)
        field "about" ~ty:Type.text;                                                (* about text for the user *)
        field ~constraints:[not_null ()] "manually_accept_follows" ~ty:Type.bool;   (* whether the user manually accepts follows *)
        field ~constraints:[not_null ()] "is_admin" ~ty:Type.bool;                  (* whether the user is an admin *)
        field ~constraints:[not_null ()] "pubkey" ~ty:Type.blob;                    (* public key for the user (X509.Public_key.t) *)
        field ~constraints:[not_null ()] "privkey" ~ty:Type.blob;                   (* private key for the user (X509.Private_key.t) *)
      ]

end

module RemoteInstance = struct

  type t = {
    id: int;
    url: string;
    last_unreachable: Ptime.t option;
  }

  let table, Expr.[id;url;last_unreachable] =
    VersionedSchema.declare_table db ~name:"remote_instance"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.int;
        field ~constraints:[unique ()] "url" ~ty:Type.text;
        field "last_unreachable" ~ty:Type.text;
      ]

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

  let table, Expr.[
    id; username; instance_id; display_name; url; inbox;
    outbox; followers; following; summary; public_key_pem
  ] = VersionedSchema.declare_table db ~name:"remote_user"
        Schema.[
          field ~constraints:[primary_key ()] "id" ~ty:Type.int;             (* internal id of the user *)
          field ~constraints:[not_null ()] "username" ~ty:Type.int;          (* username *)

          field ~constraints:[
            foreign_key ~table:RemoteInstance.table
              ~columns:Expr.[RemoteInstance.id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "instance_id" ~ty:Type.int;                                      (* reference to the instance where the user lives  *)
          field "display_name" ~ty:Type.text;                                (* display name of the user, if null then username *)
          field ~constraints:[unique (); not_null ()] "url" ~ty:Type.text;   (* url of actor (obtained by webfinger if needed) *)
          field "inbox" ~ty:Type.text;                                       (* inbox url of the user *)
          field "outbox" ~ty:Type.text;                                      (* outbox url of the user *)
          field "followers" ~ty:Type.text;                                   (* followers url of the user *)
          field "following" ~ty:Type.text;                                   (* following url of the user *)
          field "summary" ~ty:Type.text;                                     (* profile string of the user *)

          field ~constraints:[not_null ()]
            "public_key_pem" ~ty:Type.text;                                  (* public key of the user *)
        ]

end

module Actor = struct

  type t = {
    id: int;
    link_id: [`Local of int | `Remote of int]
  }

  let table, Expr.[id; local_id; remote_id] =
    VersionedSchema.declare_table db ~name:"actor"
      Schema.[
        field ~constraints:[primary_key ()]  "id" ~ty:Type.int;            (* internal id for referring to actors *)
        field ~constraints:[
          foreign_key ~table:LocalUser.table ~columns:Expr.[LocalUser.id]
            ~on_update:`RESTRICT
            ~on_delete:`RESTRICT ()                                        (* local id if a local user *)
        ] "local_id" ~ty:Type.int;
        field ~constraints:[
          foreign_key ~table:RemoteUser.table ~columns:Expr.[RemoteUser.id]
            ~on_update:`RESTRICT
            ~on_delete:`RESTRICT ()
        ] "remote_id" ~ty:Type.int;                                        (* remote id if a remote user *)
      ]

end

module Tag = struct

  type t = {
    id: int;
    name: string;
  }

  let table, Expr.[id; name] =
    VersionedSchema.declare_table db ~name:"tags"
      Schema.[
        field ~constraints:[primary_key ()] "tag_id" ~ty:Type.int;              (* tag id *)
        field ~constraints:[not_null (); unique ()] "tag_name" ~ty:Type.text;   (* tag name  *)
      ]

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

  let table, Expr.[
    id;
    public_id;
    url;
    author_id;

    is_public;
    is_follower_public;

    summary;
    content_type;
    post_source;

    published;
    raw_data
  ] =
    VersionedSchema.declare_table db ~name:"posts"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.int;                 (* internal post id, not exposed *)
        field "public_id" ~ty:Type.text;                                       (* if post by local user, assign public id *)
        field ~constraints:[not_null (); unique ()] "url" ~ty:Type.text;       (* url/id of post, if local then /api/posts/<public_id> *)
        field ~constraints:[
          not_null ();
          foreign_key ~table:Actor.table ~columns:Expr.[Actor.id]
            ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
        ] "author_id" ~ty:Type.int;                                            (* author of the post *)

        field ~constraints:[not_null ()] "is_public" ~ty:Type.bool;            (* is the post public? or only to the mentioned users *)
        field ~constraints:[not_null ()] "is_follower_public" ~ty:Type.bool;   (* is the post sent to followers mentioned users *)

        field "summary" ~ty:Type.text;                                         (* subject of the post *)
        field ~constraints:[not_null ()] "content_type" ~ty:Type.text;         (* type of the content *)
        field ~constraints:[not_null ()] "post_source" ~ty:Type.text;          (* source of the content *)

        field ~constraints:[not_null ()] "published" ~ty:Type.text;            (* date at which post was published (Ptime) *)

        field "raw_data" ~ty:Type.blob;                                        (* if by an external user, then keep json of the post (Yojson.Safe.t) *)
      ]

  module PostTo = struct
    let table, Expr.[post_id; actor_id] =
      VersionedSchema.declare_table db ~name:"post_to"
        Schema.[
          field ~constraints:[
            not_null ();
            foreign_key ~table ~columns:Expr.[id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "post_id" ~ty:Type.int;
          field ~constraints:[
            not_null ();
            foreign_key ~table:Actor.table ~columns:Expr.[Actor.id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "actor_id" ~ty:Type.int
        ]
  end

  module PostCc = struct
    let table, Expr.[post_id; actor_id] =
      VersionedSchema.declare_table db ~name:"post_cc"
        Schema.[
          field ~constraints:[
            not_null ();
            foreign_key ~table ~columns:Expr.[id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "post_id" ~ty:Type.int;
          field ~constraints:[
            not_null ();
            foreign_key ~table:Actor.table ~columns:Expr.[Actor.id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "actor_id" ~ty:Type.int
        ]    
  end

  module PostMentions = struct
    let table, Expr.[post_id; actor_id] =
      VersionedSchema.declare_table db ~name:"post_cc"
        Schema.[
          field ~constraints:[
            not_null ();
            foreign_key ~table ~columns:Expr.[id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "post_id" ~ty:Type.int;
          field ~constraints:[
            not_null ();
            foreign_key ~table:Actor.table ~columns:Expr.[Actor.id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "actor_id" ~ty:Type.int
        ]    
  end

  module PostTags = struct
    let table, Expr.[post_id; tag_id; url] =
      VersionedSchema.declare_table db ~name:"post_tags"
        Schema.[
          field ~constraints:[
            not_null ();
            foreign_key ~table ~columns:Expr.[id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "post_id" ~ty:Type.int;                                   (* post id *)
          field ~constraints:[
            not_null ();
            foreign_key ~table:Tag.table ~columns:Expr.[Tag.id]
              ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
          ] "tag_id" ~ty:Type.int;                                    (* tag id *)
          field "url" ~ty:Type.text;                                  (* href of the tag root (head to url to see all posts) if ext. *)
        ] ~constraints:Schema.[table_primary_key ["post_id"; "tag_id"]]
  end

end

module Likes = struct

  let table, Expr.[
    id;
    public_id;
    url;
    raw_data;
    published;
    post_id;
    actor_id
  ] =
    VersionedSchema.declare_table db ~name:"likes"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.int;
        field "public_id" ~ty:Type.text;
        field ~constraints:[not_null ()] "url" ~ty:Type.text;
        field "raw_data" ~ty:Type.blob;
        field ~constraints:[not_null ()] "published" ~ty:Type.text;
        field ~constraints:[
          not_null ();
          foreign_key ~table:Posts.table ~columns:Expr.[Posts.id]
            ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
        ] "post_id" ~ty:Type.int;
        field ~constraints:[
          not_null ();
          foreign_key ~table:Actor.table ~columns:Expr.[Actor.id]
            ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
        ] "actor_id" ~ty:Type.int;
      ]

end

module Follows = struct

  let table, Expr.[
    id;
    public_id;
    url;
    raw_data;
    pending;
    created;
    updated;
    author_id;
    target_id
  ] =
    VersionedSchema.declare_table db ~name:"follows"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.int;
        field "public_id" ~ty:Type.text;
        field ~constraints:[not_null ()] "url" ~ty:Type.text;
        field "raw_data" ~ty:Type.blob;
        field ~constraints:[not_null ()] "pending" ~ty:Type.bool;

        field ~constraints:[not_null ()] "created" ~ty:Type.text;
        field "updated" ~ty:Type.text;

        field ~constraints:[
          not_null ();
          foreign_key ~table:Actor.table ~columns:Expr.[Actor.id] ()
        ] "author_id" ~ty:Type.int;
        field ~constraints:[
          not_null ();
          foreign_key ~table:Actor.table ~columns:Expr.[Actor.id] ()
        ] "target_id" ~ty:Type.int;
      ]

end

