open D3

module H = D3_tyxml.Html5

let low_footer =
  let content =
    H.(p [
      pcdata "Created by " ;
      a ~a:[a_href "http://computationallyendowed.com"]
        [pcdata "Spiros Eliopoulos"]
    ])
  in
  static "footer"
  |. str attr "id" "info"
  |. H.d3' content

let view =
  seq [
    static "section"
    |. str attr "id" "todoapp"
    |- H.(d3 @@ fun s _ -> div [
        h2 [pcdata "The content"] ;
        p [ pcdata s ]
      ])
    ;
    low_footer
  ]

let d3_div = H.(div [ inject view ])
let content = H.(d3' @@ div [
    h1 [pcdata "A fabulous d3-tyxml example."] ;
    d3_div ;
  ])

let () =
  run ~node:(Dom_html.document##body) content "some content" ;
