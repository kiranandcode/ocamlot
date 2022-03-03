open Containers
module JS = Yojson.Safe

let uri_of_raw (base,path) = Uri.with_path base (String.concat "/" (List.rev path))
let assoc vls : JS.t = `Assoc vls
let string str : JS.t = `String str
let list vls : JS.t = `List vls
let uri vl : JS.t = `String (Uri.to_string (uri_of_raw vl))

let with_path path (base, paths) = (base, path :: paths)
let with_fragment fragment (base, paths) = (Uri.with_fragment base (Some fragment), paths)

let api_path config =
  (Settings.domain config, [])
  |> with_path "api"


module ActivityStreams = struct
  let context =
    "@context", list [
      string "https://www.w3.org/ns/activitystreams";
      string "https://w3id.org/security/v1"
    ]

end

  
module User = struct
  type t = Database.User.t

  let user_path config (actor: t) =
    api_path config
    |> with_path ("user/" ^ Database.User.username actor) 

  let to_json config (actor: t) =
    assoc [
      ActivityStreams.context;
      "id", uri (user_path config actor);
      "type", string "Person";
      "preferredUsername", string (Database.User.display_name actor);
      "inbox", uri (user_path config actor
                    |> with_path "inbox");
      "publicKey", assoc [
        "id", uri (user_path config actor
                   |> with_fragment "main-key");
        "owner", uri (user_path config actor);
        "publicKeyPem", string (Database.User.pubkey actor)
      ]
    ]

end
 
