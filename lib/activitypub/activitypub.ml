open Containers
module JS = Yojson.Safe

let uri_of_raw path = path
let assoc vls : JS.t = `Assoc vls
let string str : JS.t = `String str
let list vls : JS.t = `List vls
let uri vl : JS.t = `String (Uri.to_string (uri_of_raw vl))

let with_path path uri =
  let old_path = Uri.path uri in
  let old_path =
    if not @@ String.suffix ~suf:"/" old_path
    then old_path ^ "/"
    else old_path in
  Uri.with_path uri (old_path ^ path)

let with_fragment fragment uri = Uri.with_fragment uri (Some fragment)

let api_path config =
  Configuration.Url.api_base_path config


module ActivityStreams = struct

  module ContentType = struct
    let ld_json = "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""
    let activity_json = "application/activity+json"
  end
  
  let context =
    "@context", list [
      string "https://www.w3.org/ns/activitystreams";
      string "https://w3id.org/security/v1"
    ]

end

  
module User = struct
  type t = Database.LocalUser.t

  let user_path config (actor: t) =
    api_path config
    |> with_path ("user/" ^ Database.LocalUser.username actor) 

  let to_json config (actor: t) =
    assoc [
      ActivityStreams.context;
      "id", uri (user_path config actor);
      "type", string "Person";
      "preferredUsername", string (Database.LocalUser.display_name actor);
      "inbox", uri (user_path config actor
                    |> with_path "inbox");
      "publicKey", assoc [
        "id", uri (user_path config actor
                   |> with_fragment "main-key");
        "owner", uri (user_path config actor);
        "publicKeyPem", string (Database.LocalUser.pubkey actor)
      ]
    ]

end
 
module Webfinger = struct

  let profile_page url = assoc [
    "href", uri url;
    "rel", string "http://webfinger.net/rel/profile-page";
    "type", string "text/html";
  ]

  let self_link ty url = assoc [
    "href", uri url;
    "rel", string "self";
    "type", string ty
  ]    

  let activity_json_self config username =
    self_link ActivityStreams.ContentType.activity_json (Configuration.Url.user config username)
  let activitystreams_self config username =
    self_link ActivityStreams.ContentType.ld_json (Configuration.Url.user config username)

  let of_local_user config (actor: Database.LocalUser.t) =
    let username = Database.LocalUser.username actor in
    assoc [
      "subject", string (Configuration.Format.user_specifier config username);
      "aliases", list [ uri (Configuration.Url.user config username) ];
      "links", list [
        profile_page (Configuration.Url.user_profile_page config username);
        activity_json_self config username;
        activitystreams_self config username;
      ]
    ]

end
