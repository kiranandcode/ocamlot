module Password = Lib.Password

let hashing_works () =
  ignore @@ Result.get_ok (Password.hash ~pwd:"password123")

let hash_and_verify_works () =
  let pwd = "password123" in
  let encoded = Result.get_ok (Password.hash ~pwd) in
  Alcotest.(check bool) "password matches" true
  (Result.get_ok (Password.verify encoded ~pwd))
  

let () =
  Alcotest.run "test password" [
    "hashing", [
      "works", `Quick, hashing_works
    ];
    "hash and verify", [
      "works", `Quick, hash_and_verify_works
    ]
  ]
