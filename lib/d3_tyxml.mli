open D3

module Xml : Xml_sigs.T
  with type uri = string

module Svg : sig

  include Svg_sigs.Make(Xml).T

  val inject : ('a, 'a) D3.t -> _ elt

  val d3 : ('data -> int -> _ elt) -> ('data, 'data) t
  val d3' : _ elt -> ('data, 'data) t
  val enter : ('data -> int -> _ elt) -> ('data, 'data) t
  val enter' : _ elt -> ('data, 'data) t

end

module Html5 : sig

  include Html5_sigs.Make(Xml)(Svg).T

  val inject : ('a, 'a) D3.t -> _ elt

  val d3 : ('data -> int -> _ elt) -> ('data, 'data) t
  val d3' : _ elt -> ('data, 'data) t
  val enter : ('data -> int -> _ elt) -> ('data, 'data) t
  val enter' : _ elt -> ('data, 'data) t

end
