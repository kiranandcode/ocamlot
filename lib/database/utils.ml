module T = Caqti_type
module type DB = Caqti_lwt.CONNECTION
module R = Lwt_result
module Calendar = CalendarLib.Calendar


let (let*) x f = R.bind x f
let (let+) x f = R.bind (R.lift x) f
let flatten_error err = R.map_error (fun err ->  Caqti_error.show err) err


let timestamp = Caqti_type_calendar.ctime
