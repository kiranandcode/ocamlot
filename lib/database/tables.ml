open Petrol

let version_0_0_1 = VersionedSchema.version [0;0;1]
let version_0_0_2 = VersionedSchema.version [0;0;2]
let version_0_0_3 = VersionedSchema.version [0;0;3]
let version_0_0_4 = VersionedSchema.version [0;0;4]

let db = VersionedSchema.init version_0_0_3 ~name:"ocamlot"

module DreamSession = struct

  (* declare the dream table here to ensure that dream doesn't complain *)
  let table, Expr.[id;label;expires_at;payload] =
    VersionedSchema.declare_table db ~name:"dream_session"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.text;
        field ~constraints:[not_null ()] "label" ~ty:Type.text;
        field ~constraints:[not_null ()] "expires_at" ~ty:Type.real;
        field ~constraints:[not_null ()] "payload" ~ty:Type.text
      ]

end

module UserImage = struct

  let table, Expr.[path; hash] =
    VersionedSchema.declare_table db ~name:"user_images"
      Schema.[
        field ~constraints:[primary_key (); not_null (); unique ()] "path" ~ty:Type.text;
        field ~constraints:[not_null (); unique ()] "hash" ~ty:Type.blob;
      ] ~since:version_0_0_2

end

module Activity = struct

  let table, Expr.[id; raw_data] =
    VersionedSchema.declare_table db ~name:"activity"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.text;                     (* uuid of the data *)
        field ~constraints:[not_null ()] "raw_data" ~ty:Type.blob;                  (* json data (Yojson.t) *)
      ]

end

module LocalUser = struct

  let table, (Expr.[id; username; password; display_name; about; profile_picture; manually_accept_follows; is_admin; pubkey; privkey] as all_fields) =
    VersionedSchema.declare_table db ~name:"local_user"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.int;                      (* internal id, not exposed *)
        field ~constraints:[unique (); not_null ()] "username" ~ty:Type.text;       (* username *)
        field ~constraints:[not_null ()] "password" ~ty:Type.text;                  (* password hash + salt *)
        field "display_name" ~ty:Type.text;                                         (* display name - if null then username *)
        field "about" ~ty:Type.text;                                                (* about text for the user *)
        field ~constraints:[
          foreign_key ~table:UserImage.table ~columns:Expr.[UserImage.path]
             ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
        ] "profile_picture" ~ty:Type.text;                                          (* profile picture of the user *)
        field ~constraints:[not_null ()] "manually_accept_follows" ~ty:Type.bool;   (* whether the user manually accepts follows *)
        field ~constraints:[not_null ()] "is_admin" ~ty:Type.bool;                  (* whether the user is an admin *)
        field ~constraints:[not_null ()] "pubkey" ~ty:Type.blob;                    (* public key for the user (X509.Public_key.t) *)
        field ~constraints:[not_null ()] "privkey" ~ty:Type.blob;                   (* private key for the user (X509.Private_key.t) *)
      ]
      ~migrations:[version_0_0_2, [
        Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) {sql| ALTER TABLE local_user ADD COLUMN profile_picture TEXT REFERENCES user_images (path) ON DELETE RESTRICT ON UPDATE RESTRICT |sql}
      ]]

end

module RemoteInstance = struct

  let table, Expr.[id;url;last_unreachable] =
    VersionedSchema.declare_table db ~name:"remote_instance"
      Schema.[
        field ~constraints:[primary_key ()] "id" ~ty:Type.int;
        field ~constraints:[unique ()] "url" ~ty:Type.text;
        field "last_unreachable" ~ty:Type.text;
      ]

end

module RemoteUser = struct

  let table, Expr.[
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
    public_key_pem;
    profile_picture;
  ] = VersionedSchema.declare_table db ~name:"remote_user"
        Schema.[
          field ~constraints:[primary_key ()] "id" ~ty:Type.int;             (* internal id of the user *)
          field ~constraints:[not_null ()] "username" ~ty:Type.text;          (* username *)

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
          field
            "profile_picture"
            ~ty:Type.text;                                                   (* profile picture of the user *)

        ]
        ~migrations:[version_0_0_3, [
            Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) {sql| ALTER TABLE remote_user ADD COLUMN profile_picture TEXT |sql}
          ]]

end

module Actor = struct

  let table, Expr.[id; local_id; remote_id] =
    VersionedSchema.declare_table db ~name:"actor"
      Schema.[
        field ~constraints:[primary_key ()]  "id" ~ty:Type.int;            (* internal id for referring to actors *)
        field ~constraints:[
          unique ();
          foreign_key ~table:LocalUser.table ~columns:Expr.[LocalUser.id]
            ~on_update:`RESTRICT
            ~on_delete:`RESTRICT ()                                        (* local id if a local user *)
        ] "local_id" ~ty:Type.int;
        field ~constraints:[
          unique ();
          foreign_key ~table:RemoteUser.table ~columns:Expr.[RemoteUser.id]
            ~on_update:`RESTRICT
            ~on_delete:`RESTRICT ()
        ] "remote_id" ~ty:Type.int;                                        (* remote id if a remote user *)
      ]

end

module Tag = struct

  let table, Expr.[id; name] =
    VersionedSchema.declare_table db ~name:"tags"
      Schema.[
        field ~constraints:[primary_key ()] "tag_id" ~ty:Type.int;              (* tag id *)
        field ~constraints:[not_null (); unique ()] "tag_name" ~ty:Type.text;   (* tag name  *)
      ]

end

module Posts = struct

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
        field ~constraints:[unique ()] "public_id" ~ty:Type.text;                                       (* if post by local user, assign public id *)
        field ~constraints:[not_null (); unique ()] "url" ~ty:Type.text;       (* url/id of post, if local then /api/posts/<public_id> *)
        field ~constraints:[
          not_null ();
          foreign_key ~table:Actor.table ~columns:Expr.[Actor.id]
            ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
        ] "author_id" ~ty:Type.int;                                            (* author of the post *)

        field ~constraints:[not_null ()] "is_public" ~ty:Type.bool;            (* is the post public? or only to the mentioned users *)
        field ~constraints:[not_null ()] "is_follower_public" ~ty:Type.bool;   (* is the post sent to followers mentioned users *)

        field "summary" ~ty:Type.text;                                         (* subject of the post *)
        field ~constraints:[not_null ()] "content_type" ~ty:Type.int;         (* type of the content *)
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
        ] ~constraints:Schema.[
            table_unique ["post_id"; "actor_id"]
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
        ] ~constraints:Schema.[
            table_unique ["post_id"; "actor_id"]
          ]
  end

  module PostMentions = struct
    let table, Expr.[post_id; actor_id] =
      VersionedSchema.declare_table db ~name:"post_mention"
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
        ~constraints:Schema.[
            table_unique ["post_id"; "actor_id"]
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


module Reboosts = struct

  let table, Expr.[
    id;
    public_id;
    url;
    raw_data;
    published;
    post_id;
    actor_id
  ] =
    VersionedSchema.declare_table db ~name:"reboosts"
      Schema.[
        field ~constraints:[primary_key ()] "reboost_id" ~ty:Type.int;
        field "reboost_public_id" ~ty:Type.text;
        field ~constraints:[not_null ()] "reboost_url" ~ty:Type.text;
        field "reboost_raw_data" ~ty:Type.blob;
        field ~constraints:[not_null ()] "reboost_published" ~ty:Type.text;
        field ~constraints:[
          not_null ();
          foreign_key ~table:Posts.table ~columns:Expr.[Posts.id]
            ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
        ] "reboost_post_id" ~ty:Type.int;
        field ~constraints:[
          not_null ();
          foreign_key ~table:Actor.table ~columns:Expr.[Actor.id]
            ~on_update:`RESTRICT ~on_delete:`RESTRICT ()
        ] "reboost_actor_id" ~ty:Type.int;
      ] ~since:version_0_0_4

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

