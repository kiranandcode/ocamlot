[@@@warning "-32"]

let decode_string = Activitypub.Decode.decode_string

let decode_and_print file (decode,print) =
  match decode_string decode (In_channel.with_open_bin file In_channel.input_all) with
  | Ok res -> print_endline (Format.asprintf "%a" print res)
  | Error err -> failwith err

let create (dec,pr) = Activitypub.(Decode.create dec, Types.pp_create pr)
let announce (dec,pr) = Activitypub.(Decode.announce dec, Types.pp_announce pr)
let like = Activitypub.(Decode.like, Types.pp_like)
let note = Activitypub.(Decode.note, Types.pp_note)
let follow = Activitypub.(Decode.follow, Types.pp_follow)
let user = Activitypub.(Decode.person, Types.pp_person)

let obj = Activitypub.(Decode.obj, Types.pp_obj)

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | ["like"; file] ->
    decode_and_print file like
  | ["create-note"; file] ->
    decode_and_print file (create note)
  | ["follow"; file] ->
    decode_and_print file follow
  | ["user"; file] ->
    decode_and_print file user
  | ["announce"; file] ->
    decode_and_print file obj
  | ["obj"; file] ->
    decode_and_print file obj
  | _ -> failwith ("unsupported parameters: " ^ (String.concat ", " args))
