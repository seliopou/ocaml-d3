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

(** js_of_ocaml bindings for the D3.js library. *)

type ('a, 'b) t
(** The type of a D3 operation that assumes that its parent has data of type
    ['a] bound to it, and which itself can act as a context with data of type
    ['b] bound to it. *)

type ('a, 'b) fn = Dom.node Js.t -> 'a -> int -> 'b
(** A type alias for the sorts of functions that can be used to construct
    {!attr}, {!classed}, {!style}, and {!property}, selections below. *)

val data : ?all:bool -> string -> ('a -> int -> 'b list) -> ('a, 'b) t
(** [select ~all selector fn] selects descendant elements that match the
    specified selector string, for each element in the selection, and use [fn]
    to bind data to it.

    If [all] is [true] (the default), all the elements are selected matching
    [selector] are selected, otherwise only the first matching element in
    document traversal order will be selected.
*)

val datum : ?all:bool -> string -> ('a -> int -> 'b) -> ('a, 'b) t
(** Similar to {!data}, but does not computer enter and exit selections. *)

val attr : string -> ('a, string) fn -> ('a, 'a) t
(** [attr name f] sets the [name] attribute to the specified value on all
    selected elements. The value is determined by calling [f] for each element,
    passing it the current DOM element, the datum attached to the DOM element,
    and the element's index in the selection.

    {{:https://github.com/mbostock/d3/wiki/Selections#attr}D3.js docs} *)

val classed : string -> ('a, bool) fn -> ('a, 'a) t
(** [classed class f] sets whether or not [class] is associated with the
    selected elements. This is determined by calling [f] for each element,
    passing it the current DOM element, the datum attached to the DOM element,
    and the element's index in the selection.

    This operator is a convenience routine for setting the ["class"] attribute;
    it understands that the ["class"] attribute is a set of tokens separated by
    spaces. If you want to set several classes at once use a space-separated
    list of class names as the first argument.

    {{:https://github.com/mbostock/d3/wiki/Selections#classes}D3.js docs} *)

val style : string -> ('a, string) fn -> ('a, 'a) t
(** [style name f] sets the [name] CSS style property to the specified value on
    all selected elements. The value is determined by calling [f] for each element,
    passing it the current DOM element, the datum attached to the DOM element,
    and the element's index in the selection.

    {{:https://github.com/mbostock/d3/wiki/Selections#style}D3.js docs} *)

val property : string -> ('a, 'b Js.t ) fn -> ('a, 'a) t
(** [property name f] sets the [name] property to the specified value on all
    selected elements. The value is determined by calling [f] for each element,
    passing it the current DOM element, the datum attached to the DOM element,
    and the element's index in the selection.

    {{:https://github.com/mbostock/d3/wiki/Selections#property}D3.js docs} *)

val text : ('a, string) fn -> ('a, 'a) t
(** [text f] sets the text content to the specified value on all selected
    elements. The value is determined by calling [f] for each element, passing
    it the current DOM element, the datum attached to the DOM element, and the
    element's index in the selection.

    The [text] operator is based on the
    {{:http://www.w3.org/TR/DOM-Level-3-Core/core.html#Node3-textContent}textContent}
    property; setting the text content will replace any existing child elements.

    {{:https://github.com/mbostock/d3/wiki/Selections#text}D3.js docs} *)

val html : ('a, string) fn -> ('a, 'a) t
(** [html f] sets the inner HTML content to the specified value on all selected
    elements. The value is determined by calling [f] for each element, passing
    it the current DOM element, the datum attached to the DOM element, and the
    element's index in the selection.

    The [html] operator is based on the
    {{:http://dev.w3.org/html5/spec-LC/apis-in-html-documents.html#innerhtml}innerHTML}
    property; setting the inner HTML content will replace any existing child
    elements. Also, you may prefer to use the {!append} operator to create HTML
    content in a data-driven way; this operator is intended for when you want a
    little bit of HTML, say for rich formatting.

    {{:https://github.com/mbostock/d3/wiki/Selections#html}D3.js docs} *)

val append : string -> ('a, 'a) t
(** [append name] appends a new [name] element as the last child of each
    element in the current selection, returning a new selection containing the
    appended elements. Each new element inherits the data of the current
    elements in the same manner as {!select}.

    {{:https://github.com/mbostock/d3/wiki/Selections#append}D3.js docs} *)

val remove : ('a, 'a) t
(** [remove] removes the elements in the current selection from the current
    document. Returns the current selection (the same elements that were
    removed) which are now "off-screen", detached from the DOM.

    {{:https://github.com/mbostock/d3/wiki/Selections#remove}D3.js docs} *)

val static : string -> ('a, 'a) t
(** [static name] appends a new [name] element as the last child of each
    element in the current selection, if [name] child element does not already
    exist. If an [name] element already does exist, then it will return a
    selection containing just that element.

    Note that you can use multiple [static] operators to create elements with
    the same name.

{[nest (data "body" @@ fun d _i -> [d])
  [ static "div"
    |. text (fun _ _ _ -> "first div")
  ; static "div"
    |. text (fun _ _ _ -> "second div") ] ]}

    This is not part of the D3.js API. *)

(** {2 Data Binding} *)

val enter : string -> ('a, 'a) t
(** [enter s] gets the enter selection and append [s] to it. The enter
    selection is composed of placeholder nodes for each data
    element for which no corresponding existing DOM element was found in the
    current selection. This operation can only be composed below an update
    selection, which is produced by the {!data} operator.

    {{:https://github.com/mbostock/d3/wiki/Selections#enter}D3.js docs} *)

val update : ('a, 'a) t
(** [update] is a NOOP operation. It leaves the parent selection unchanged and
    has no side-effects.

    This is not part of the D3.js API. *)

val exit : ('a, 'a) t
(** [exit] returns the exit selection: existing DOM elements in the current
    selection for which no new data element was found. This operation can only
    be composed below an update selection, which is produced by the {!data}
    operator.

    {{:https://github.com/mbostock/d3/wiki/Selections#exit}D3.js docs} *)

(** {2 Selection manipulation} *)

val filter : ('a, bool) fn -> ('a, 'a) t
(** [filter f] returns a new selection that contains only the elements for
    which [f] evaluates to [true].

    {{:https://github.com/mbostock/d3/wiki/Selections#filter}D3.js docs} *)

val sort : ('a -> 'a -> int) -> ('a, 'a) t
(** [sort f] sorts the elements in the current selection according to the
    comparator function, and then re-inserts the document elements to match.

   {{:https://github.com/mbostock/d3/wiki/Selections#sort}D3.js docs} *)

val each : ('a, unit) fn -> ('a, 'a) t
(** [each f] invokes [f] for each element in the current selection.

   {{:https://github.com/mbostock/d3/wiki/Selections#each}D3.js docs} *)

(** {2 Composition operators} *)

val (|.)  : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val (<.>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** [x |. y] is the equivalent of method chaining operators [x] and [y] in
    JavaScript. [<.>] is an alias for [|.]. *)

val (|-)  : ('a, 'b) t -> ('b, 'c) t -> ('a, 'b) t

val chain : ('a, 'a) t list -> ('a, 'a) t
val seq   : ('a, 'a) t list -> ('a, 'a) t

val nest  : ('a, 'b) t -> ('b, 'b) t list -> ('a, 'b) t
(** [nest x ys] is equivalent to [x |- seq ys]. *)

(** {2 Casting} *)

val str : (string -> ('a, string) fn -> ('a, 'a) t) -> string -> string -> ('a, 'a) t
val int : (string -> ('a, string) fn -> ('a, 'a) t) -> string -> int    -> ('a, 'a) t
val flt : (string -> ('a, string) fn -> ('a, 'a) t) -> string -> float  -> ('a, 'a) t
(** [str attr x y] will set the [x] attribute to the constant string value [y].
    The [int] and [flt] functions do the same thing except with constant values
    of different types. These convenience functions should be used with the
    {!attr}, and {!style} functions when dealing with constant values. That way
    you can write code like this:

{[data "rect" (fun d _ -> [d])
|. int attr  "height" height
|. int attr  "width"  height
|. str style "fill"   "blue"]}

    rather than this:

{[data "rect" (fun d _ -> [d])
|. attr  "height" (fun _ _ _ -> string_of_int height)
|. attr  "width"  (fun _ _ _ -> string_of_int width)
|. style "fill"   (fun _ _ _ -> "blue")]} *)


(** Event handlers *)
module E : sig
  type ('event, 'a) handler = 'event Js.t -> 'a -> int -> unit
  (** The type of an event handler. The handler expects an event of type
      ['event] that it can interact with via the js_of_ocaml API, as well as
      data of type ['a] to be associated with the DOM node that produced the
      event. *)

  (** {2 Mouse events} *)

  val click       : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val dblclick    : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mousedown   : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mouseup     : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mouseover   : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mousemove   : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mouseout    : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t

  (** {2 Keyboard events} *)

  val keypress    : (Dom_html.keyboardEvent, 'a) handler -> ('a, 'a) t
  val keydown     : (Dom_html.keyboardEvent, 'a) handler -> ('a, 'a) t
  val keyup       : (Dom_html.keyboardEvent, 'a) handler -> ('a, 'a) t

  (** {2 Drag-and-drop events} *)

  val dragstart   : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragend     : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragenter   : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragover    : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragleave   : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val drag        : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val drop        : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t

  (** {2 Miscellaneous events} *)

  val input       : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val timeupdate  : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val change      : (Dom_html.event, 'a) handler -> ('a, 'a) t

  val focus       : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val blur        : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val scroll      : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val submit      : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val select      : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val mousewheel  : (Dom_html.event, 'a) handler -> ('a, 'a) t

  (** {2 Generic events} *)

  val handle : string -> (Dom_html.event, 'a) handler -> ('a, 'a) t
end

val run : ?node:#Dom.node Js.t -> ('a, _) t -> 'a -> unit
(** [run ?node op datum] binds [datum] to [node] and runs [op] on that element.
    If [node] is not provided, then the current document node will be used
    instead.

    This is the only way to run a D3.t operation. It can be used in a variety
    of contexts, however its indended use is in an event loop of an
    application, along these lines:

{[
let main () =
  let model = ref Model.init in
  D3.run "body" !model view;
  Stream.iter events (fun e ->
    model := handle e model;
    D3.run "body" !model view)
;;

main ()]} *)


module Unsafe : sig

  val to_ : (< .. > Js.t -> < .. > Js.t) -> ('a, 'b) t
  val from_ : ('a, 'b) t -> (< .. > Js.t -> < .. > Js.t)

  val enter : ('a, 'a) t
  (** See
      {{:https://github.com/mbostock/d3/wiki/Selections#enter}D3.js docs}
  *)

  val select : string -> ('a, 'a) t
  (** See
      {{:https://github.com/mbostock/d3/wiki/Selections#select}D3.js docs}
  *)

  val selectAll : string -> ('a, 'a) t
  (** See
      {{:https://github.com/mbostock/d3/wiki/Selections#selectAll}D3.js docs}
  *)

  val data : ('a -> int -> 'b list) -> ('a, 'b) t
  (** See
      (** {{:https://github.com/mbostock/d3/wiki/Selections#data}D3.js docs} *)
  *)

  val datum : ('a -> int -> 'b) -> ('a, 'b) t
  (** See
      {{:https://github.com/mbostock/d3/wiki/Selections#datum}D3.js docs}
  *)
end
