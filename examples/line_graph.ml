type twodim_point = {
    i_x: float
  ; i_y: float }

type line = {
    index: int (* starts at 0 *)
  ; line: twodim_point list
  ; highlight: bool }

type lines = line list

type paddings = {
    top: float
  ; right: float
  ; left: float
  ; bottom: float }

open Js

module D3_libs = struct
  let d3 () = Unsafe.global##.d3

  let get_line () = (d3 ())##line

  let get_scale s = match s with
    | `scaleLinear -> (d3 ())##scaleLinear ()

  let get_curve c = match c with
    | `curveBasis -> fun x -> (d3 ())##curveBasis x
    | `curveLinear -> fun x -> (d3 ())##curveLinear x

  let get_axis c = match c with
    | `Bottom -> fun x -> (d3 ())##axisBottom x
    | `Left -> fun x -> (d3 ())##axisLeft x

end

module Line = struct
  class type data_point = object
    method x : float readonly_prop
    method y : float readonly_prop
  end
  type scaleU
  class type scale = object
    method range : float js_array t -> scale t meth
    method domain : float js_array t -> scaleU t meth
  end
  class type axis = object
    method scale : scale t -> axis t meth
    method tickSize : int -> axis t meth
    method tickSubdivide : bool -> axis t meth
  end
  class type line = object
    method x : (line t, data_point t -> int -> float) meth_callback
      -> line t meth
    method y : (line t, data_point t -> int -> float) meth_callback
      -> line t meth
    method curve : (line t, 'a) meth_callback -> line t meth
  end
end

module Event = struct
  type t =
    | Highlight of int
    | NoHighlight

  let highlight (m:lines) i =
    List.map
      (fun l -> let highlight = l.index = i in { l with highlight })
      m

  let handle t m : lines = match t with
    | Highlight i -> highlight m i
    | NoHighlight -> highlight m (-1)

end

module View = struct
  let palette_loop i =
    let choices = [|
        (* these colors are grouped by 2: strong tone followed by soft *)
        "#1f78b4"; "#a6cee3"; (* blue *)
        "#33a02c"; "#b2df8a"; (* green *)
        "#e31a1c"; "#fb9a99"; (* red *)
        "#ff7f00"; "#fdbf6f"; (* orange *)
        "#6a3d9a"; "#cab2d6"; (* purple *)
      |]
    in
    choices.(i mod 10)

  let make_range ~range ~domain =
    let obj = D3_libs.get_scale `scaleLinear in
    ignore @@ obj##range (Js.array [| fst range; snd range |]);
    ignore @@ obj##domain (Js.array [| fst domain; snd domain |]);
    (fun (x:float) -> Js.Unsafe.fun_call obj [| Js.Unsafe.inject x |]), obj

  let make_axis pos range_o m =
    let a = (D3_libs.get_axis pos) range_o in
    Js.Unsafe.fun_call a [|Js.Unsafe.inject m|]

  let data_to_js (l: line) : Line.data_point t js_array t =
    let curve_to_js { line=c } =
      c
      |> List.map
        (fun (x: twodim_point) : Line.data_point t -> object%js
            val x = x.i_x
            val y = x.i_y
          end)
      |> Array.of_list
      |> array
    in
    curve_to_js l

  let line xrange yrange m: Line.line t =
    let open D3_libs in
    let l = get_line () in
    ignore @@ l##x (fun (o:Line.data_point t) _ -> xrange o##.x);
    ignore @@ l##y (fun (o:Line.data_point t) _ -> yrange o##.y);
    ignore @@ l##curve (fun x -> get_curve `curveLinear x);
    Js.Unsafe.fun_call l [|Js.Unsafe.inject m|]

  let get_size size_ratio node =
    (* take 100% of available width in [node] and relative height *)
    let w = (Js.coerce node Dom_html.CoerceTo.div (fun _ -> assert false))
        ##.clientWidth
    in
    let h = int_of_float @@ (float_of_int w) *. size_ratio in
    w, h

  let items ~size_ratio ~node ~pad ~data_preview f =
    let open D3 in
    let w, h = get_size size_ratio node in
    let min_max l : float*float = match l with
      | [] -> 0., 100.
      | a :: [] -> a, a
      | a :: b -> let ls = List.fast_sort compare (a::b) in
          (List.hd ls), List.hd @@ List.rev ls
    in
    let map_tuple f = fun (a, b) -> (f a), f b in
    let xdom, ydom =
      map_tuple min_max @@
      List.split @@ List.map (fun { i_x; i_y } -> i_x, i_y) @@
      List.flatten @@ List.map (fun { line } -> line) data_preview
    in
    let ydom = (min (fst ydom) 0.), snd ydom in
    let xrange = 0., (float_of_int @@ w) -. pad.right -. pad.left in
    let xrange, xrange_o = make_range ~range:xrange ~domain:xdom in
    let yrange = ((float_of_int h) -. pad.top -. pad.bottom), 0. in
    let yrange, yrange_o = make_range ~range:yrange ~domain:ydom in
    let stroke_width = "3" in
    selectAll "path"
    |. data (fun m _ -> m)
    |- nest enter [
        append "path"
        |. str attr "fill" "none"
        |. attr "stroke" (fun _ { index } _ -> palette_loop index)
        |. str attr "stroke-linejoin" "round"
        |. str attr "stroke-linecap" "round"
        |. str attr "stroke-width" stroke_width
        |. E.mouseover (fun _ { index } _ -> f (Event.Highlight index))
        |. E.mouseout (fun _ _ _ -> f (Event.NoHighlight))
        |. attr_obj "d" (fun _ m _ -> line xrange yrange @@ data_to_js m)
      ]
    |- nest enter [
        static "g"
        |. attr "transform" (fun _ _ _ -> Format.sprintf "translate(0,%.1f)"
            ((float_of_int h) -. pad.top -. pad.bottom))
        |. call (fun m _ -> make_axis `Bottom xrange_o m)
      ]
    |- nest enter [
        static "g"
        |. call (fun m _ -> make_axis `Left yrange_o m)
      ]
    |- nest update [
        transition
        |. fun_call "duration" (fun _ _ _ -> "200")
        |. style "stroke-width"
          (fun _ { highlight } _ -> if highlight then "5" else stroke_width)
      ]

  let make ~size_ratio ~node ~pad ~data_preview f =
    let open D3 in
    let w, h = get_size size_ratio node in
    static "svg"
    |. int attr "width" w
    |. int attr "height" h
    |. style "padding"
      (fun _ _ _ -> Format.sprintf "%.1f %.1f %.1f %.1f"
        pad.top pad.right pad.bottom pad.left)
    |. static "g"
    |- items ~size_ratio ~node ~data_preview ~pad f
end

let n_random_curves n =
  let rec range debut fin tl =
    if fin > debut
    then range debut (fin - 1) (fin :: tl)
    else debut :: tl
  in
  List.map
    (fun i ->
      {
        highlight = false;
        index = i-1;
        line = List.map
          (fun x -> {
            i_x = float_of_int x;
            i_y = 0.5 *. (float_of_int i) *. (float_of_int x ** 2.)
             })
          (range 1 100 [])
      }
    )
    (range 1 n [])

let make_stream () =
  let stream, push = Lwt_stream.create () in
  stream, (fun x -> push (Some x)), (fun () -> push None)

let fold_stream ~stream ~handler ~node ~view ~data =
  Lwt_stream.fold_s
    (fun e m ->
      let m' = handler e m in
      D3.run ~node view m';
      Lwt.return m')
    stream data

let async_draw_update ~size_ratio ~node ~pad data () =
  let stream, push, _ = make_stream () in
  let view = View.make ~size_ratio ~node ~pad ~data_preview:data push in
  D3.run ~node view data;
  Lwt.return @@ fold_stream ~handler:(Event.handle) ~node ~view ~data ~stream

let _ =
  let size_ratio = 0.8 in
  let node = match (Dom_html.getElementById_opt "main-div") with
    | Some d -> d
    | _ -> assert false
  in
  let pad = {top=40.; bottom=40.; right=20.; left=50.} in
  let data = n_random_curves 6 in
  Lwt_js_events.async (async_draw_update ~size_ratio ~node ~pad data)
