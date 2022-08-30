let t_cost = 3
let m_cost = 64 * 1024
let parallelism = 2
let hash_len = 32
let salt = "Iyè«Q[á2ÑČ÷`ğ?"
let salt_len = String.length salt
let kind = Argon2.I
let version = Argon2.VERSION_13

let hash ~pwd =
  let encoded_len =
    Argon2.encoded_len
      ~t_cost ~m_cost ~parallelism
      ~salt_len ~hash_len
      ~kind:Argon2.I in
  let hash_result =
    Argon2.hash
      ~t_cost ~m_cost ~parallelism ~pwd ~salt ~kind
      ~hash_len ~encoded_len ~version in
  Result.map snd hash_result
  |> Result.map_error (fun e -> Argon2.ErrorCodes.message e)

let verify encoded ~pwd =
  Argon2.verify ~encoded ~pwd ~kind:Argon2.I
  |> function
    Ok _ as res -> res
  | Error Argon2.ErrorCodes.VERIFY_MISMATCH -> Ok false
  | Error e -> Error ( Argon2.ErrorCodes.message e)
