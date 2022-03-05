let parse_date = Parser.parse_date
let parse_date_exn = Parser.parse_date_exn

let to_utc_string t =
  let www = 
    Ptime.weekday t |> function `Sat -> "Sat" |`Fri -> "Fri" |`Mon -> "Mon" |`Wed -> "Wed" |`Sun -> "Sun" |`Tue -> "Tue" |`Thu -> "Thu" in
  let ((yyyy,mmm,dd), ((hh,mm,ss), _)) = Ptime.to_date_time t  in
  let mmm = match mmm with
    | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr" | 5 -> "May"
    | 6 -> "Jun" | 7 -> "Jul" | 8 -> "Aug" | 9 -> "Sep" | 10 -> "Oct"
    | 11 -> "Nov" | 12 -> "Dec" | _ -> assert false in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    www dd mmm yyyy hh mm ss

