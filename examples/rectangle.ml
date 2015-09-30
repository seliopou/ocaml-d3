open D3

type dims =
  { width : int; height : int }

let rec unfold i f =
  if i <= 0 then []
  else f (i-1) :: unfold (i-1) f

let make_dims len =
  let f i = { width = 300; height = (i+1) * 50}, i*300, 20 in
  unfold len f

let view =
  let open Svg in
  let svg =
    static "svg"
    |. attr "width" (fun _ l _ -> string_of_int @@ l * 300)
    |. attr "height"  (fun _ l _ -> string_of_int 300 )
  in
  let rect (dims, pos, padding) _ =
    rect ~a:[
      a_fill (`Color ("black", None)) ;
      a_x (float @@ pos + padding, None) ;
      a_y (float padding, None) ;
      a_width (float @@ dims.width - 2 * padding, None) ;
      a_height (float @@ dims.height - 2 * padding, None) ;
    ] []
  in
  svg |. (
    selectAll "rect"
    |. data (fun i _ -> make_dims i)
    |- nest enter [with_svg rect]
    |- nest exit [remove]
  )
;;

let _ =
  run ~node:(Dom_html.document##body) view 4
