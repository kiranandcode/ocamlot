module D = Decoders_yojson.Safe.Decode

let timestamp =
  let open D in
  let* time = string in
  match Ptime.of_rfc3339 time |> Ptime.rfc3339_error_to_msg with
  | Ok (t, _, _) -> succeed t
  | Error `Msg err -> fail err

let singleton_or_list dec =
  D.(one_of ["singleton", (dec >|= fun v -> [v]);
             "list", list dec;
             "null", null >|= fun () -> []])  

let lossy_list_of dec =
  let open D in
  list (one_of ["known", (dec >|= fun v -> `Value v); "unknown", value >|= fun v -> `Raw v])


let constant ?msg target =
  let open D in
  let* str = string in
  if String.equal str target
  then succeed ()
  else match msg with
    | None -> fail (Printf.sprintf "expected %s received %s" target str)
    | Some msg -> fail (Printf.sprintf msg str)

let field_or_default field' decoder default =
  let open D in
  let+ field = field_opt field' decoder in
  Option.value ~default field

let list_ignoring_unknown ty =
  let open D in
  list (maybe ty) >|= fun v -> List.filter_map Fun.id v

let items obj =
  let open D in
  one_of [
    ("ordered items",
     let* items = field "orderedItems" (list obj) in
     succeed (true, items));
    "items",
    let* items = field "items" (list obj) in
    succeed (false, items)
  ]
