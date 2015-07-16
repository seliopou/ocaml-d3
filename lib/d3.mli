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

type ('a, 'b) t
type ('a, 'b) fn = Dom.node Js.t -> 'a -> int -> 'b

val select    : string -> ('a, 'a) t
val selectAll : string -> ('a, 'a) t

val attr     : string -> ('a, string  ) fn -> ('a, 'a) t
val classed  : string -> ('a, bool    ) fn -> ('a, 'a) t
val style    : string -> ('a, string  ) fn -> ('a, 'a) t
val property : string -> ('a, 'b Js.t ) fn -> ('a, 'a) t

val text : ('a, string) fn -> ('a, 'a) t
val html : ('a, string) fn -> ('a, 'a) t

val append : string -> ('a, 'a) t
val static : string -> ('a, 'a) t
val remove : ('a, 'a) t

val datum : ('a -> int -> 'b list) -> ('a, 'b) t
val data  : ('a -> int -> 'b list) -> ('a, 'b) t

val enter  : ('a, 'a) t
val update : ('a, 'a) t
val exit   : ('a, 'a) t

val filter : ('a, bool) fn -> ('a, 'a) t
val sort   : ('a -> 'a -> int) -> ('a, 'a) t

val (|.)  : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val (<.>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val (|-)  : ('a, 'b) t -> ('b, 'c) t -> ('a, 'b) t

val chain : ('a, 'a) t list -> ('a, 'a) t
val seq   : ('a, 'a) t list -> ('a, 'a) t
val nest  : ('a, 'b) t -> ('b, 'b) t list -> ('a, 'b) t

val str : (string -> ('a, string) fn -> ('a, 'a) t) -> string -> string -> ('a, 'a) t
val int : (string -> ('a, string) fn -> ('a, 'a) t) -> string -> int    -> ('a, 'a) t
val flt : (string -> ('a, string) fn -> ('a, 'a) t) -> string -> float  -> ('a, 'a) t

module E : sig
  type ('event, 'a) handler = 'event Js.t -> 'a -> int -> unit

  val click       : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val dblclick    : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mousedown   : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mouseup     : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mouseover   : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mousemove   : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t
  val mouseout    : (Dom_html.mouseEvent, 'a) handler -> ('a, 'a) t

  val keypress    : (Dom_html.keyboardEvent, 'a) handler -> ('a, 'a) t
  val keydown     : (Dom_html.keyboardEvent, 'a) handler -> ('a, 'a) t
  val keyup       : (Dom_html.keyboardEvent, 'a) handler -> ('a, 'a) t

  val dragstart   : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragend     : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragenter   : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragover    : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val dragleave   : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val drag        : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t
  val drop        : (Dom_html.dragEvent, 'a) handler -> ('a, 'a) t

  val input       : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val timeupdate  : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val change      : (Dom_html.event, 'a) handler -> ('a, 'a) t

  val focus       : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val blur        : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val scroll      : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val submit      : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val select      : (Dom_html.event, 'a) handler -> ('a, 'a) t
  val mousewheel  : (Dom_html.event, 'a) handler -> ('a, 'a) t

  val handle : string -> (Dom_html.event, 'a) handler -> ('a, 'a) t
end

val run : string -> 'a -> ('a, _) t -> unit
