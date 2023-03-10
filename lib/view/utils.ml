module H = Tyxml.Html

type link = {
  url: string;
  text: string;
  form: string option;
}

let rec intersperse c ls =
  match ls with
  | [] -> []
  | h :: [] -> [h]
  | h :: t ->
    h :: (c ()) :: intersperse c t

let month_to_string = function
  | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr" | 5 -> "May" | 6 -> "Jun" | 7 -> "Jul" | 8 -> "Aug" | 9 -> "Sept" | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec" | _ -> "Unk"

let day_terminator = fun n ->
  match (n mod 10) with
  | 1 -> "st"
  | 2 -> "nd"
  | 3 -> "rd"
  | _ -> "th"

let pp_date: Format.formatter -> Ptime.date -> unit =
  fun fmt (year, month, day) ->
  Format.fprintf fmt "%d%s of %s %d" day (day_terminator day) (month_to_string month) year

let div class_ elts = H.div ~a:[ H.a_class [class_]] elts

let div_flex elts =
  div "flex" elts

let spacer () =
  div "flex-grow-1" []

let hidden_fields fields =
  List.map (fun (key, value) ->
    H.input
      ~a:[H.a_name key; H.a_input_type `Hidden;
          H.a_value value] ()
  ) fields
