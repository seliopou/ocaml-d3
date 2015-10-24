(*----------------------------------------------------------------------------
    Copyright (c) 2015 Spiridon Eliopoulos.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

type 'a s = S

type ('a, 'b) t = 'a s -> 'b s
type ('a, 'b) fn = Dom.node Js.t -> 'a -> int -> 'b

let d3_select arg =
  Js.Unsafe.(meth_call global##d3 "select" [| inject arg |])
;;

let name_call (meth:string) (name:string) f =
  let open Js.Unsafe in
  let name = Js.string name in
  fun cxt -> meth_call cxt meth [| inject name; inject f |]
;;

let const_call (meth:string) arg cxt =
  Js.(Unsafe.meth_call cxt meth [| Unsafe.inject arg |])
;;

let thunk_call (meth:string) cxt =
  Js.Unsafe.meth_call cxt meth [| |]
;;

let mb = Js.wrap_meth_callback

let raw_select    name = const_call "select"    (Js.string name)
let raw_selectAll name = const_call "selectAll" (Js.string name)

let attr     name f = name_call "attr"     name (mb (fun this d i () -> Js.string (f this d i)))
let classed  name f = name_call "classed"  name (mb (fun this d i () -> Js.bool   (f this d i)))
let style    name f = name_call "style"    name (mb (fun this d i () -> Js.string (f this d i)))
let property name f = name_call "property" name (mb (fun this d i () -> f this d i))

let text f = const_call "text" (mb (fun this d i () -> Js.string (f this d i)))
let html f = const_call "html" (mb (fun this d i () -> Js.string (f this d i)))

let append name         = const_call "append" (Js.string name)
let remove : ('a, 'a) t = thunk_call "remove"

let raw_datum f = const_call "datum" (fun d i -> f d i)
let raw_data  f = const_call "data"  (fun d i -> Js.array (Array.of_list (f d i)))

let raw_enter  : ('a, 'a) t = thunk_call "enter"
let update : ('a, 'a) t = fun x -> x
let exit   : ('a, 'a) t = thunk_call "exit"

let filter f = const_call "filter" (mb (fun this d i () -> Js.bool (f this d i)))
let sort   f = const_call "sort"   f
let each   f = const_call "each"   (mb (fun this d i () -> (f this d i)))

let str f name x = f name (fun _ _ _ -> x)
let int f name x = str f name (string_of_int x)
let flt f name x = str f name (string_of_float x)

let _seq u v =
  fun cxt ->
    let _ = u cxt in
    v cxt

let (|.)  u v = fun cxt -> v (u cxt)
let (<.>) u v = u |. v
let (|-) u v = u <.> (_seq v update)

let chain l =
  let rec loop l acc =
    match l with
    | []         -> acc
    | hd::[]     -> acc <.> hd
    | hd::sd::[] -> acc <.> hd <.> sd
    | hd::tl     -> loop tl (acc <.> hd)
  in
  loop l update

let seq l =
  let rec loop l acc =
    match l with
    | []     -> acc
    | hd::[] -> _seq acc hd
    | hd::tl -> loop tl (_seq acc hd)
  in
  loop l update

let nest top lst =
  top |- seq lst

let gensym = object
  val mutable id = 0

  method next =
    let x = id in
    id <- x + 1;
    x
end

let static name =
  let gensym_  = string_of_int gensym#next in
  let selector = name ^ "[ocaml-d3-gensym=\"" ^ gensym_ ^ "\"]"in
  let f this _ _ =
    let this = d3_select this in
    let that = raw_select selector this in
    if thunk_call "size" that = 0 then
      ignore ((append name <.> str attr "ocaml-d3-gensym" gensym_) this)
  in
  _seq (each f) (raw_select selector)
;;


let enter s = raw_enter <.> append s

let data ?(all=true) s fn =
  if all
  then raw_selectAll s <.> raw_data fn
  else raw_select s <.> raw_data fn

let datum ?(all=true) s fn =
  if all
  then raw_selectAll s <.> raw_datum fn
  else raw_select s <.> raw_datum fn

module E = struct
  type ('event, 'a) handler = 'event Js.t -> 'a -> int -> unit

  let _handler name f =
    let f' d i = f Js.Unsafe.global##d3##event d i in
    fun cxt -> name_call "on" name f' cxt
  ;;

  let click       f = _handler "click"      f
  let dblclick    f = _handler "dblclick"   f
  let mousedown   f = _handler "mousedown"  f
  let mouseup     f = _handler "mouseup"    f
  let mouseover   f = _handler "mouseover"  f
  let mousemove   f = _handler "mousemove"  f
  let mouseout    f = _handler "mouseout"   f

  let keypress    f = _handler "keypress"   f
  let keydown     f = _handler "keydown"    f
  let keyup       f = _handler "keyup"      f

  let input       f = _handler "input"      f
  let timeupdate  f = _handler "timeupdate" f
  let change      f = _handler "change"     f

  let dragstart   f = _handler "dragstart"  f
  let dragend     f = _handler "dragend"    f
  let dragenter   f = _handler "dragenter"  f
  let dragover    f = _handler "dragover"   f
  let dragleave   f = _handler "dragleave"  f
  let drag        f = _handler "drag"       f
  let drop        f = _handler "drop"       f

  let focus       f = _handler "focus"      f
  let blur        f = _handler "blur"       f
  let scroll      f = _handler "scroll"     f
  let submit      f = _handler "submit"     f
  let select      f = _handler "select"     f
  let mousewheel  f = _handler "mousewheel" f

  let handle name f = _handler name f
end

let run ?(node=Js.Unsafe.global##document##documentElement) t data =
  let cxt =
    let open Js.Unsafe in
    meth_call
      (d3_select node)
      "datum" [| inject data |]
  in
  ignore (t cxt)
;;

module Unsafe = struct

  let enter = raw_enter
  let select = raw_select
  let selectAll = raw_selectAll
  let data = raw_data
  let datum = raw_datum

end
