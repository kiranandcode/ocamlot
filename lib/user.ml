module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type
module L = Caqti_lwt

let list_comments =
  let query =
    R.collect T.unit T.(tup2 int string)
      {|SELECT id, text FROM comment|} in
  fun (module Db: DB) ->
    let%lwt comments_or_error = Db.collect_list query () in
    L.or_fail comments_or_error
