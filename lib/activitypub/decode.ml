open Containers
open Common

let decode_string enc vl = D.decode_string enc vl |> Result.map_err D.string_of_error

let id = D.(one_of ["string", string; "id", field "id" string])

let ordered_collection_page obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"Expected OrderedCollectionPage (received %s)" "OrderedCollectionPage"
  and* id = field "id" string
  and* next = field_opt "next" id
  and* prev = field_opt "prev" id
  and* part_of = field_opt "partOf" string
  and* total_items = field_opt "totalItems" int
  and* (is_ordered, items) = items obj in
  succeed ({id; next; prev; part_of; total_items; is_ordered; items}: _ Types.ordered_collection_page)

let ordered_collection obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"Expected OrderedCollection (received %s)" "OrderedCollection"
  and* id = field_opt "id" string
  and* total_items = field "totalItems" int
  and* contents =
    one_of [
      "items", map (fun v -> `Items v) (items obj);
      "first", map (fun v -> `First v) (field "first" (ordered_collection_page obj))
    ] in
  succeed ({id; total_items; contents}: _ Types.ordered_collection)

let mention =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Mention (received %s)" "Mention"
  and* href = field "href" string
  and* name = field "name" string in
  succeed ({ty=`Mention; href;name} : Types.tag)

let hashtag =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Hashtag (received %s)" "Hashtag"
  and* href = field "href" string
  and* name = field "name" string in
  succeed ({ty=`Hashtag; href;name}: Types.tag)

let tag =
  let open D in
  let* ty = field "type" string in
  match ty with
  | "Mention" -> mention
  | "Hashtag" -> hashtag
  | _ -> fail (Printf.sprintf "unknown tag %s" ty)

let undo obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Undo (received %s)" "Undo"
  and* id = field "id" string
  and* actor = field "actor" id
  and* published = field_opt "published" timestamp
  and* obj = field "object" obj
  and* raw = value in
  succeed ({id;published;actor;obj;raw}: _ Types.undo)

let like =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Like (received %s)" "Like"
  and* id = field "id" string
  and* actor = field "actor" id
  and* published = field_opt "published" timestamp
  and* obj = field "object" id
  and* raw = value in
  succeed ({id; actor; published; obj; raw}: Types.like)

let tombstone =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Tombstone (received %s)" "Tombstone"
  and* id = field "id" string in
  succeed id

let delete obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Delete (received %s)" "Delete"
  and* id = field "id" string
  and* actor = field "actor" id
  and* published = field_opt "published" timestamp
  and* obj = field "object" obj
  and* raw = value in
  succeed ({id;published;actor;obj;raw}: _ Types.delete)

let block =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Block (received %s)" "Block"
  and* id = field "id" string
  and* obj = field "object" string
  and* published = field_opt "published" timestamp
  and* actor = field "actor" id
  and* raw = value in
  succeed ({id;published;obj;actor;raw}: Types.block)

let accept obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Accept (received %s)" "Accept"
  and* id = field "id" string
  and* actor = field "actor" id
  and* published = field_opt "published" timestamp
  and* obj = field "object" obj
  and* raw = value in
  succeed ({id;published;actor;obj;raw}: _ Types.accept)

let public_key =
  let open D in
  let* id = field "id" string
  and* owner = field "owner" string
  and* pem = field "publicKeyPem" string in
  succeed ({id;owner;pem}: Types.public_key)

let attachment =
  let open D in
  let* media_type = field_opt "mediaType" string
  and* name = field_opt "name" string
  and* type_ = field_opt "type" string
  and* url = field "url" string in
  succeed ({media_type;name;type_;url}: Types.attachment)


let person =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Person (received %s)" "Person"
  and* id = field "id" string
  and* name = field_opt "name" string
  and* url = field_or_default "url" (nullable string) None
  and* preferred_username = field_opt "preferredUsername" string
  and* inbox = field "inbox" string
  and* outbox = field "outbox" string
  and* summary = field_opt "summary" string
  and* public_key = field "publicKey" public_key
  and* manually_approves_followers =
    field_or_default "manuallyApprovesFollowers" bool false
  and* discoverable = field_or_default "discoverable" bool false
  and* followers = field_opt "followers" string
  and* following = field_opt "following" string
  and* icon = maybe (at ["icon";"url"] string)
  and* raw = value in
  succeed ({
    id;
    name;
    url;

    preferred_username;

    inbox;
    outbox;

    summary;

    public_key;

    manually_approves_followers;

    discoverable;
    followers;
    following;
    icon;
    raw;
  }: Types.person)

let note = 
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Note (received %s)" "Note"
  and* id = field "id" string
  and* actor = one_of ["actor", field "actor" id; "attributed_to", field "attributedTo" id]
  and* attachment = field_or_default "attachment" (singleton_or_list attachment) []
  and* to_ = field "to" (singleton_or_list string)
  and* in_reply_to = field_or_default "inReplyTo" (nullable string) None
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* content = field "content" string
  and* source = field_opt "source"
                  (one_of ["string", string; "multi-encode", field "content" string])
  and* summary = field_or_default "summary" (nullable string) None
  and* sensitive = field_or_default "sensitive" (nullable bool) None
  and* published = field_opt "published" timestamp
  and* tags = field_or_default "tag" (lossy_list_of tag) []
  and* raw = value in
  succeed ({ id; actor; attachment; in_reply_to; to_; cc;
             sensitive=Option.value ~default:false sensitive;
             content; source; summary; tags; published; raw }: Types.note)

let follow =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected create object (received %s)" "Follow"
  and* actor = field "actor" id
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* to_ = field_or_default "to" (singleton_or_list string) []
  and* id = field "id" string
  and* object_ = field "object" id
  and* state = field_opt "state" (string >>= function "pending" -> succeed `Pending
                                                    | "cancelled" -> succeed `Cancelled
                                                    | _ -> fail "unknown status")
  and* raw = value in
  succeed ({actor; cc; to_; id; object_; state; raw}: Types.follow)

let announce obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected create object (received %s)" "Announce"
  and* actor = field "actor" id
  and* id = field "id" string
  and* published = field_opt "published" timestamp
  and* to_ = field "to" (singleton_or_list string)
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* obj = field "object" obj
  and* raw = value in
  succeed ({id; published; actor; to_; cc; obj; raw}: _ Types.announce)

let create obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected create object (received %s)" "Create"
  and* id = field "id" string
  and* actor = field "actor" id
  and* direct_message = field_or_default "direct" bool false
  and* published = field_opt "published" timestamp
  and* to_ = field_or_default "to" (singleton_or_list string) []
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* obj = field "object" obj
  and* raw = value in

  succeed ({
    id; actor; published;
    to_; cc;
    direct_message;
    obj;
    raw;
  }: _ Types.create)

let core_obj () =
  let open D in
  let* ty = field_opt "type" string in
  match ty with
  | Some "Person" -> person >|= fun v -> `Person v
  | Some "Follow" -> follow >|= fun v -> `Follow v
  | Some "Note" -> note >|= fun v -> `Note v
  | Some "Block" -> block >|= fun v -> `Block v
  | Some "Like" -> like >|= fun v -> `Like v
  | None -> string >|= fun v -> `Link v
  | Some ev -> fail ("unsupported event" ^ ev)

let core_obj = core_obj ()

let event (enc: Types.core_obj D.decoder) : Types.obj D.decoder =
  let open D in
  let* ty = field "type" string in
  match ty with
  | "Create" -> create enc >|= fun v -> `Create v
  | "Accept" -> accept enc >|= fun v -> `Accept v
  | "Undo" -> undo enc >|= fun v -> `Undo v
  | "Delete" -> delete enc >|= fun v -> `Delete v
  | "Announce" -> announce enc >|= fun v -> `Announce v
  | _ -> fail "unsupported event"

let obj : Types.obj D.decoder =
  D.one_of [
    "core_obj", core_obj;
    "core_obj event", (event core_obj)
  ]

module Webfinger = struct

  let ty =
    let open D in
    string >>= function
    | str when String.prefix ~pre:Constants.ContentType.html str ->
      succeed `Html
    | str when String.prefix ~pre:Constants.ContentType.plain_json str ->
      succeed `Json
    | str when String.prefix ~pre:Constants.ContentType.activity_json str ->
      succeed `ActivityJson
    | str when String.prefix ~pre:Constants.ContentType.ld_json_activity_streams str ->
      succeed `ActivityJsonLd
    | _ ->
      fail "unsupported self link type"

  let self =
    let open D in
    let* ty = field "type" ty
    and* href = field "href" string in
    succeed @@ Types.Webfinger.Self (ty, href)

  let profile_page =
    let open D in
    let* ty = field "type" ty
    and* href = field "href" string in
    succeed @@ Types.Webfinger.ProfilePage (ty, href)

  let ostatus_subscribe =
    let open D in
    let* template = field "template" string in
    succeed @@ Types.Webfinger.OStatusSubscribe template

  let link =
    let open D in
    let* rel = field "rel" string in
    match rel with
    | "self" -> self
    | str when String.equal str Constants.Webfinger.ostatus_rel ->
      ostatus_subscribe
    | str when String.equal str Constants.Webfinger.profile_page ->
      profile_page
    | _ -> fail "unsupported link relation"

  let query_result =
    let open D in
    let* subject = field "subject" string
    and* aliases = field "aliases" (list string)
    and* links = field "links" (list_ignoring_unknown link) in

    succeed Types.Webfinger.{subject;aliases;links}

end

module Nodeinfo = struct

  let software =
    let open D in
    let* name = field "name" string
    and* version = field "version" string in
    succeed @@ Types.Nodeinfo.{name;version}

  let usage_users =
    let open D in
    let* total = field_or_default "total" int 0
    and* active_month = field_or_default "activeMonth" int 0
    and* active_half_year = field_or_default "activeHalfyear" int 0 in
    succeed @@ Types.Nodeinfo.{total; active_month; active_half_year}

  let usage =
    let open D in
    let* users = field "users" usage_users 
    and* local_posts = field_or_default "localPosts" int 0 in
    succeed @@ Types.Nodeinfo.{users; local_posts}

  let t =
    let open D in
    let* software = field "software" software
    and* protocols = field_or_default "protocols" (list string) []
    and* inbound_services = field_or_default "services" (field_or_default "inbound" (list string) []) []
    and* outbound_services = field_or_default "services" (field_or_default "outbound" (list string) []) []
    and* usage = field "usage" usage
    and* open_registrations = field_or_default "openRegistrations" bool false 
    and* metadata = field_opt "metadata" value
    and* raw = value in
    succeed @@ Types.Nodeinfo.{software;protocols;inbound_services;outbound_services;usage;open_registrations;metadata;raw}


end
