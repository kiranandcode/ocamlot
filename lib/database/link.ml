open Utils

type 'a tag = int64 -> (module DB) -> ('a, string) R.t

type +'a t = Int64.t * 'a tag

let resolve : 'a t -> (module DB) -> ('a, string) R.t = fun (id,fn) db -> fn id db
