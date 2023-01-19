module H = Tyxml.Html

let rec inline_to_html_ignore_links ?(allowed_html=(fun _ -> false))  : (string * string) list Omd.inline -> _ = function
  | Omd.Concat (_, elts) -> List.concat_map (inline_to_html_ignore_links ~allowed_html) elts
  | Omd.Text (_, txt) ->  [(H.txt txt)]
  | Omd.Emph (_, txt) ->
    [H.i (inline_to_html  ~allowed_html txt)]
  | Omd.Strong (_, txt) ->
    [H.b (inline_to_html  ~allowed_html txt)]
  | Omd.Code (_, txt) ->
    [H.code [H.txt txt]]
  | Omd.Hard_break _ -> [H.br ()]
  | Omd.Soft_break _ -> []
  | Omd.Image (_, _)
  | Omd.Html (_, _)  -> []
  | Omd.Link (_, _) -> []
and inline_to_html ?(allowed_html=(fun _ -> false))  : (string * string) list Omd.inline -> _ = function
  | Omd.Concat (_, elts) -> List.concat_map (inline_to_html ~allowed_html) elts
  | Omd.Text (_, txt) ->  [(H.txt txt)]
  | Omd.Emph (_, txt) ->
    [H.i (inline_to_html ~allowed_html txt)]
  | Omd.Strong (_, txt) ->
    [H.b (inline_to_html ~allowed_html txt)]
  | Omd.Code (_, txt) ->
    [H.code [H.txt txt]]
  | Omd.Hard_break _ -> [H.br ()]
  | Omd.Soft_break _ -> []
  | Omd.Image (_, _) -> []
  | Omd.Html (_, html)  -> if allowed_html html then [Tyxml.Html.Unsafe.data html] else []
  | Omd.Link (_, { label; destination; title=_ }) ->
    let contents = inline_to_html_ignore_links  ~allowed_html label in
    [H.a ~a:[H.a_href destination] contents]

let rec markdown_block_to_html ?allowed_html : Omd.attributes Omd.block -> _ = function
  | Omd.Paragraph (_, txt) ->
    [H.p (inline_to_html ?allowed_html txt)]
  | Omd.List (_, Omd.Ordered (_, _), _, list_items) ->
    [H.ol
      (List.map (fun elts ->
         H.li (List.concat_map (markdown_block_to_html ?allowed_html) elts)
       ) list_items)]
  | Omd.List (_, Omd.Bullet (_), _, list_items) ->
    [H.ul
      (List.map (fun elts ->
         H.li (List.concat_map (markdown_block_to_html ?allowed_html) elts)
       ) list_items)]
  | Omd.Blockquote (_, contents) ->
    [H.blockquote (List.concat_map (markdown_block_to_html ?allowed_html) contents)]
  | Omd.Thematic_break _ -> [H.br ()]
  | Omd.Heading (_, 1, txt) -> [H.h1 (inline_to_html ?allowed_html txt)]
  | Omd.Heading (_, 2, txt) -> [H.h2 (inline_to_html ?allowed_html txt)]
  | Omd.Heading (_, 3, txt) -> [H.h3 (inline_to_html ?allowed_html txt)]
  | Omd.Heading (_, 4, txt) -> [H.h4 (inline_to_html ?allowed_html txt)]
  | Omd.Heading (_, 5, txt) -> [H.h5 (inline_to_html ?allowed_html txt)]
  | Omd.Heading (_, _, txt) -> [H.h6 (inline_to_html ?allowed_html txt)]
  | Omd.Code_block (_, _, code) ->
    [H.code [H.txt code]]
  | Omd.Html_block (_, txt) -> [Tyxml.Html.Unsafe.data txt]
  | Omd.Table _ -> []
  | Omd.Definition_list (_, _) -> []

let markdown_to_html  ?allowed_html : Omd.doc -> _ = fun doc -> List.concat_map (markdown_block_to_html  ?allowed_html) doc
