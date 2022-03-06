
(* hash a password *)
val hash : pwd:string -> (string, string) result

(* verify a password against a stored hash *)
val verify : string -> pwd:string -> (bool, string) result
