open Containers

module ContentType = struct
  let ld_json_activity_streams = "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""

  let ld_json = "application/ld+json"
  let activity_json = "application/activity+json"
  let plain_json = "application/json"
  let html = "text/html"
  let any = "*/*"

  let content_types = [
    ld_json_activity_streams, `JSON;
    ld_json, `JSON;
    activity_json, `JSON;
    plain_json, `JSON;
    html, `HTML;
    any, `HTML;
  ]

  let of_string content_type =
    List.find_opt
      (fun (str, _) ->
         String.prefix ~pre:str content_type) content_types
    |> Option.map snd


end

module Webfinger = struct
  let json_rd = "application/jrd+json"

  let self_rel = "self"
  let ostatus_rel = "http://ostatus.org/schema/1.0/subscribe"
  let profile_page = "http://webfinger.net/rel/profile-page"
end

module ActivityStreams = struct

  let context : string * Yojson.Safe.t =
    "@context", `List [
      `String "https://www.w3.org/ns/activitystreams";
      `String "https://w3id.org/security/v1"
    ]

end
