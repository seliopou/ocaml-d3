open D3

type dims = 
  { width : int; height : int }

let view dims padding =
  let svg =
    append "svg"
    |. int attr "width"  dims.width
    |. int attr "height" dims.height
  in
  let rect =
    append "rect"
    |. str attr "fill"   "black"
    |. int attr "width"  (dims.width - 2 * padding)
    |. int attr "height" (dims.height - 2 * padding)
    |. int attr "x"      padding
    |. int attr "y"      padding
  in
  select "body"
  |. svg <.> rect
;;

let _ =
  run (view { width = 300; height = 300 } 20) ()
