  $ ./decoder.exe follow resources/pleroma-follow.json
  { Types.id =
    "https://pleroma.ocamlot.xyz/activities/1fec6aaa-f8d5-47e8-85c6-1e8c4e1f9d14";
    actor = "https://pleroma.ocamlot.xyz/users/random"; cc = [];
    to_ = ["https://testing.ocamlot.xyz/users/orangemerin"];
    object_ = "https://testing.ocamlot.xyz/users/orangemerin";
    state = (Some `Pending);
    raw =
    {
      "@context": [
        "https://www.w3.org/ns/activitystreams",
        "https://pleroma.ocamlot.xyz/schemas/litepub-0.1.jsonld",
        { "@language": "und" }
      ],
      "actor": "https://pleroma.ocamlot.xyz/users/random",
      "bcc": [],
      "bto": [],
      "cc": [],
      "id": "https://pleroma.ocamlot.xyz/activities/1fec6aaa-f8d5-47e8-85c6-1e8c4e1f9d14",
      "object": "https://testing.ocamlot.xyz/users/orangemerin",
      "state": "pending",
      "to": [ "https://testing.ocamlot.xyz/users/orangemerin" ],
      "type": "Follow"
    } }
