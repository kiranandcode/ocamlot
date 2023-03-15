open Containers
module APConstants = Activitypub.Constants

let log = Logging.add_logger "error.display"

let (let*) x f = Lwt_result.bind x f
let (let+) x f = Lwt_result.map f x
let (>>) x f = Lwt.map f x
let (>>=) x f = Lwt_result.bind x f
let map_err f x = Lwt_result.map_error f x
let return_error f v = Lwt.return (Result.map_err f v)
let return v = Lwt.return v
let return_ok v = Lwt.return_ok v
let lift_opt ~else_:else_ = function
  | None -> Error (else_ ())
  | Some v -> Ok v
let get_opt ~else_ opt = Lwt.return (lift_opt ~else_ opt)
let lift_pure res = Lwt.map Result.return res
let form_data key form =
  List.Assoc.get ~eq:String.equal key form
  |> Option.to_result (Format.sprintf "missing field %s" key)

let form_data_present key form =
  List.Assoc.get ~eq:String.equal key form
  |> function None -> Ok false | Some _ -> Ok true

let map_list f ls =
  Lwt.map Result.flatten_l (Lwt_list.map_s f ls)

let iter_list f ls =
  let rec flatten_result_list (ls: _ Result.t list) : _ Result.t =
    match ls with
    | [] -> Ok ()
    | Ok _ :: tl -> flatten_result_list tl
    | Error err :: _ -> Error err in
  Lwt.map flatten_result_list (Lwt_list.map_s f ls)


let map_list_suppressing_errors ?tag f ls =
  let tag = match tag with None -> "" | Some tag -> tag ^ ": " in
  Lwt_list.map_s f ls
  |> Lwt.map (List.filter_map (function
      | Ok v -> Some v
      | Error err ->
        let _, msg, details = Error_handling.extract_error_details err in
        log.error (fun f -> f "%s %s" tag msg);
        log.debug (fun f -> f "%s details - %s" tag details);
        None
    ))
  |> lift_pure

let iter_list_suppressing_errors ?tag f ls =
  let tag = match tag with None -> "" | Some tag -> tag ^ ": " in
  Lwt_list.map_s f ls
  |> Lwt.map (List.iter (function
      | Ok _ -> ()
      | Error err ->
        let _, msg, details = Error_handling.extract_error_details err in
        log.error (fun f -> f "%s %s" tag msg);
        log.debug (fun f -> f "%s %s" tag details);
        ()
    ))
  |> lift_pure



(* Result monad for validation *)
module VResult = struct
  include Result

  let lift err = Result.map_err List.return err

  let lift_pair_fst f (l,r) =
    match f l with
    | Ok l -> Ok (l,r)
    | Error _ as err -> err
  let lift_pair_snd f (l,r) =
    match f r with
    | Ok r -> Ok (l,r)
    | Error _ as err -> err

  let lift_result_check f = function
      None -> Ok None
    | Some data -> f data |> Result.map Option.some

  let (and*) x y = match x,y with
      Ok x, Ok y -> Ok (x,y)
    | Error l, Error r -> Error (l @ r)
    | Error e, _ | _, Error e -> Error e

  let check_input_size field max_size data =
    if String.length data > max_size
    then Error (
      `InputTooLarge (
        field,
        max_size,
        "[" ^
        string_of_int (String.length data) ^
        "]" ^
        String.take 100 data ^
        "..."
      ))
    else Ok data

  let ensure msg cond = if cond then Ok () else Error [msg]

end

