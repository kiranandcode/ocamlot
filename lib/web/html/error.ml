module H = Tyxml.Html

let error ?details message =
  H.div ~a:[H.a_class ["main"]] @@ List.concat [
    [
      Pure.grid_row [ Pure.grid_col [
        H.div ~a:[H.a_class ["error-title"]] [
          H.h1 [H.txt "Server Error: Failed to complete your request"];
        ];
      ]];
      Pure.grid_row [ Pure.grid_col [
        H.div ~a:[H.a_class ["error-content"]] @@
        match details with
        | None -> 
          [
            H.txt ("Failed to complete your request with error: " ^ message);
            H.br ();
            H.txt "Please try again....";
          ]
        | Some details ->
          [
            H.txt ("Failed to complete your request with error: " ^ message);
            H.pre (details |> String.split_on_char '\n' |> List.map H.txt)
          ]
      ]];
    ];
  ];
