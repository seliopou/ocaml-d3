open D3

module Xml = struct

  module W = Xml_wrap.NoWrap

  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type 'a attr_wrap = 'a

  type uri = string
  let uri_of_string s = s
  let string_of_uri s = s
  type aname = string

  class type biggest_event = object
    inherit Dom_html.event
    (* inherit Dom_html.mouseEvent *)
    (* inherit Dom_html.keyboardEvent *)
  end

  type biggest_event_handler = (biggest_event, unit) E.handler
  type event_handler = (Dom_html.event, unit) E.handler
  type mouse_event_handler = (Dom_html.event, unit) E.handler
  type keyboard_event_handler = (Dom_html.event, unit) E.handler
  type attrib_k =
    | Event of biggest_event_handler
    | Attr of string
  type attrib = aname * attrib_k

  let attrib f name s : attrib = name,Attr (f s)

  let float_attrib = attrib Xml_print.string_of_number
  let int_attrib = attrib string_of_int
  let string_attrib = attrib (fun x -> x)
  let space_sep_attrib = attrib (String.concat " ")
  let comma_sep_attrib = attrib (String.concat ",")

  let uri_attrib = attrib uri_of_string
  let uris_attrib = attrib (String.concat " ")

  let event_handler_attrib name value =
    name,Event (value :> biggest_event_handler)
  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    name,Event (value :> biggest_event_handler)
  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    name,Event (value :> biggest_event_handler)

  let attr_to_d3 (aname, v) = match v with
    | Attr v ->
      str attr aname v
    | Event f ->
      E.handle aname (fun e _ i -> f e () i)


  type elt = < > Js.t -> < > Js.t
  type ename = string

  let magic ( f : ('data, 'data) t ) : elt = Unsafe.from_ f
  let d3 (f: elt) : ('data, 'data) t = Unsafe.to_ f

  let empty () = fun x -> x
  let comment _ = fun x -> x

  let pcdata s =
    let f _ _ _ = s in
    magic (text f)

  let encodedpcdata = pcdata

  let entity e =
    let entity = Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";")) in
    pcdata @@ Js.to_string entity

  let leaf ?(a=[]) elem =
    let attrs = seq (List.map attr_to_d3 a) in
    magic (append elem |- attrs)

  let node ?(a=[]) elem children =
    let attrs = seq (List.map attr_to_d3 a) in
    let children = seq (List.map d3 children) in
    magic (append elem |- attrs |- children)

  let cdata s = pcdata s
  let cdata_script s = cdata s
  let cdata_style s = cdata s

end

(* (\* The enter selection doesn't define "each", so we iterate manually instead. *\) *)
(* let each (type data) (fn : _ -> data -> int -> 'b s) (ctx : data s) : data s = *)
(*   let groups : 'a #Js.js_array Js.t = Js.Unsafe.coerce ctx in *)
(*   groups##forEach (fun group j _ -> *)
(*     group##forEach (fun node i _ -> *)
(*       Js.Optdef.iter node (fun n -> *)
(*         let x = Js.Unsafe.get n "__data__" in *)
(*         print_endline @@ Obj.magic x ; *)
(*         print_endline @@ Obj.magic node ; *)
(*         ignore @@ fn n x i *)
(*       ) *)
(*     ) *)
(*   ) ; *)
(*   ctx *)

let d3_select arg =
  Js.Unsafe.(meth_call global##d3 "select" [| inject arg |])

let rebind (fn : 'data -> int -> _) : ('data, 'data) t =
  let f node d i =
    let node = d3_select node in
    ignore @@ (fn d i) node
  in
  each f

module type S = sig
  type 'a elt
  val toelt : 'a elt -> Xml.elt
end

module Make (M : S) = struct

  let inject = Xml.magic

  let d3 (fn : 'data -> int -> _ M.elt) : ('data, 'data) t =
    let f d i = M.toelt (fn d i) in
    rebind f

  let d3' e = d3 @@ fun _ _ -> e

end

module Svg = struct
  module M = Svg_f.Make(Xml)
  include M
  include Make(M)
end

module Html5 = struct
  module M = Html5_f.Make(Xml)(Svg)
  include M
  include Make(M)
end
