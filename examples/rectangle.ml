open D3

type layout =
  { width : int; height : int; padding : int }

let view =
  let s = string_of_int in
  (* Note that the view no longer takes any arguments for dimensions or
   * padding. Rather, those parameters are passed via the datum that's bound to
   * the parent context. The [svg] operation access the height and width by
   * using a function as an argument to [attr]. That way, if the width or heigh
   * change, the attribute will be updated. Similarly for the width, height, x,
   * and y attributes for [rect]. 
   *
   * Also, note that the [append] operation has been replaced with [static].
   * This ensures that on redraws, no additional elements will be appended. *)
  let svg =
    static "svg"
    |. attr "width"  (fun _ d _ -> s d.width)
    |. attr "height" (fun _ d _ -> s d.height)
  in
  let rect =
    static "rect"
    |. str attr "fill"   "black"
    |. attr "width"  (fun _ d _ -> s (d.width  - 2 * d.padding))
    |. attr "height" (fun _ d _ -> s (d.height - 2 * d.padding))
    |. attr "x" (fun _ d _ -> s d.padding)
    |. attr "y" (fun _ d _ -> s d.padding)
  in
  svg <.> rect
;;

let layout =
  { width = 300; height = 300; padding = 20 }

let go layout =
  run "body" layout view

(* You can make the view reactive to changes in layout by mapping the [go]
 * function over a [layout React.signal] *)
let _ =
  go layout
