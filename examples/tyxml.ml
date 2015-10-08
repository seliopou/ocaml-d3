open D3

module T = D3.Tyxml.Make(Tyxml_js.To_dom)
module H = Tyxml_js.Html5

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
  |. T.html (fun _ _ _ -> content)

let view =
  seq [
    static "section"
    |. str attr "id" "todoapp"
    |- T.html (fun _ _ _ -> H.(p [pcdata "Content!"]))
    ;
    low_footer
  ]

let d3_div = H.div []
let content = H.[
    h1 [pcdata "A fabulous d3-tyxml example."] ;
    d3_div ;
  ]

let () =
  Tyxml_js.Register.body content ;
  T.run d3_div () view
