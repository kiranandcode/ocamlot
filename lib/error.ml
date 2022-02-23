
type t = [
  | `Argon2 of Argon2.ErrorCodes.t
  | Caqti_error.t
]

let pp fmt (v: t) =
  (match v with
    | `Argon2 error -> Format.fprintf fmt "%s" @@ Argon2.ErrorCodes.message error
    | #Caqti_error.t as error -> Caqti_error.pp fmt error
  )
let show = Format.asprintf "%a" pp

let caqti (v: Caqti_error.t) : t = (v :> t)
let argon2 v : t = `Argon2 v

let wrap_caqti_error fn =
  Lwt_result.map_err (fun e -> caqti e) fn
  
