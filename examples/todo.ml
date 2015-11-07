(** The Model

    This module contains data types and related operations for modeling the
    state of the application. *)
module Model = struct

  (** Ways to filter items in the interface. *)
  type filter =
    | All       (** Show all items. *)
    | Active    (** Show active (incomplete) items. *)
    | Completed (** Show completed items. *)

  let string_of_filter : filter -> string = function
    | All -> "All"
    | Active -> "Active"
    | Completed -> "Completed"

  (** The model type for the Todo application.

      The entire state of the application is contained in this type, and should
      be considered the authoritative record of the application's state. *)
  type t =
    { input   : string                (** The input for a new task description.
                                          This should remain consistent with 
                                          the [<input>] element the user
                                          actually interacts with *)
    ; items   : (string * bool) list  (** The items in the todo list. The first
                                          tuple component is the description and
                                          the second indicates whether the item
                                          is complete ([true]) or incomplete
                                          ([false]). *)
    ; filter  : filter                (** The filter that should be applied to
                                          the view. *)
    ; editing : (string * int) option (** When an item's being edited, this
                                          contains its description and index
                                          into the {!items} list. *)

    ; toggle  : bool                  (** Toggle button state. All items should
                                          be marked complete if [true], and
                                          incomplete otherwise. Only read when
                                          modified. *)
    }

  let items_to_json is =
    `List List.(map (fun (msg, state) -> `List [`String msg; `Bool state]) is)

  let items_of_json = function
    | `List lst ->
      List.map (function
        | `List [`String msg; `Bool state] -> (msg, state)
        | _                                -> failwith "deserialization error: clear 'todos-ocaml-d3' from local storage")
      lst
    | _ ->
      failwith "deserialization error: clear 'todos-ocaml-d3' from local storage"

  let init =
    { input = ""; items = []; filter = All; editing = None; toggle = false }

  let change_input t input =
    { t with input }

  let change_filter t filter =
    { t with filter }

  let update_status t i b =
    { t with items = List.mapi (fun j (t, s) -> (t, if i = j then b else s)) t.items }

  let toggle_status t =
    { t with toggle = not t.toggle
           ; items  = List.map (fun (d, _) -> (d, not t.toggle)) t.items }

  let update_description t i d =
    { t with items = List.mapi (fun j (t, s) -> ((if j = i then d else t), s)) t.items }

  let start_edit t i =
    { t with editing = Some(fst (List.nth t.items i), i) }

  let change_edit t d =
    match t.editing with
    | None       -> assert false
    | Some(_, i) -> { t with editing = Some(d, i) }

  let commit_edit t =
    match t.editing with
    | None       -> assert false
    | Some(s, i) -> { update_description t i s with editing = None }

  let cancel_edit t =
    { t with editing = None }

  let add_item t =
    { t with input = ""; items = (t.input, false)::t.items }

  let delete_item t i =
    let idx = ref 0 in
    { t with items = List.filter (fun _ -> idx := !idx + 1; !idx -1 <> i) t.items }

  let undone t =
    List.(length (filter (fun (_, b) -> not b) t.items))

  let toggle t =
    { t with toggle = not t.toggle }
end

(** The Events

    This module contains data types for application-level events and a handler.
    These events make no reference to how the user triggered the event, but
    instead describe low-level UI interactions in the high-level language of
    the application domain. For example, the {!Edit} event is triggered by a
    double click on a particular DOM element. Rather than reporting that
    low-level event, the view translates into something that makes sense within
    the context of the application: begin editing the i-th item of the list. *)
module Event = struct

  (** The type of application-level events that the Todo application must
      handle. The view code below must map DOM events to this data type.

      Note that because events are represented as variants of a single type,
      event handling code must be put in one place.  *)
  type t =
    | AddInput
    | ChangeInput of string
    | Delete of int
    | Edit of int
    | ChangeEdit of string
    | CommitEdit
    | CancelEdit
    | Check of int
    | Uncheck of int
    | Filter of Model.filter
    | Toggle

  (** [handle event model] returns a new model that reflects the changes
      encoded in [event]. *)
  let handle t m =
    match t with
      | AddInput      -> Model.add_item m
      | ChangeInput i -> Model.change_input  m i
      | Filter      f -> Model.change_filter m f
      | Toggle        -> Model.toggle_status m
      | Delete  index -> Model.delete_item m index
      | Edit    index -> Model.start_edit  m index
      | ChangeEdit  d -> Model.change_edit m d
      | CommitEdit    -> Model.commit_edit m
      | CancelEdit    -> Model.cancel_edit m
      | Check   index -> Model.update_status m index true
      | Uncheck index -> Model.update_status m index false
end

let ifilter_map ~f xs =
  let rec loop f i xs =
    match xs with
    | []     -> []
    | x::xs' ->
      begin match f x i with
      | None    -> loop f (i + 1) xs'
      | Some x' -> x' :: (loop f (i + 1) xs')
      end
  in
  loop f 0 xs
;;

module View = struct
  open D3

  type item =
    { text : string; completed : bool; editing : bool }

  let items_of_model m =
    let display =
      let open Model in
      match m.filter with
      | All       -> fun _ -> true
      | Active    -> fun x -> not x
      | Completed -> fun x -> x
    in
    ifilter_map m.Model.items ~f:(fun (text, completed) i ->
      match display completed, m.Model.editing with
      | true, Some(text', j) ->
        let text = if i = j then text' else text in
        Some { text; completed; editing = i = j }
      | true, None -> Some { text; completed; editing = false }
      | false, _   -> None)

  let items k =
    data "li" (fun m _ -> items_of_model m)
    |- nest (enter "li")
        [ E.dblclick (fun _ _ i -> k (Event.Edit i)) ]
    |- nest update
       [ classed "editing"   (fun _ m i -> m.editing)
       ; classed "completed" (fun _ m i -> m.completed)
       ; static "div"
         |. str attr "class" "view"
         |. seq
            [ static "input"
              |. str attr "class" "toggle"
              |. str attr "type" "checkbox"
              |. property "checked" (fun _ m i -> Js.bool m.completed)
              |. E.click (fun _ m i ->
                  if m.completed
                    then k (Event.Uncheck i)
                    else k (Event.Check   i))
            ; static "label"
              |. text (fun _ m _ -> m.text)
            ; static "button"
              |. str attr "class" "destroy"
              |. E.click (fun _ _ i -> k (Event.Delete i)) ]
          ; static "input"
            |. str attr "class" "edit"
            |. property "value" (fun _ m i -> Js.string m.text)
            |. E.input (fun e _ _ ->
                match Js.Opt.to_option e##target with
                | None   -> assert false
                | Some t ->
                  let i = Js.coerce t Dom_html.CoerceTo.input (fun _ -> assert false) in
                  k (Event.ChangeEdit (Js.to_string i##value)))
            |. E.keyup (fun e _ _ ->
                 match e##keyCode with
                 | 27 -> k Event.CancelEdit (* ESC_KEY   *)
                 | 13 -> k Event.CommitEdit (* ENTER_KEY *)
                 | _  -> ())
            |. E.blur (fun _ _ _ -> k Event.CancelEdit) ]
    |- chain
       [ exit <.> remove ]

  let filters k =
    data "li" (fun m _ ->
      let open Model in
      [ (All      , m.filter = All      , "#/")
      ; (Active   , m.filter = Active   , "#/active")
      ; (Completed, m.filter = Completed, "#/completed")
      ])
    |- nest (enter "li")
        [ append "a"
          |. attr "href" (fun _ (_, _, href) _ -> href)
          |. text (fun _ (f, _, _) _ -> Model.string_of_filter f) ]
    |- nest update
       [ data "a" (fun d _ -> [d])
         |. classed "selected" (fun _ (_, active, _) _ -> active) ]
         |. E.click (fun _ (f,_,_) _ -> k (Event.Filter f))
    |- nest exit
       [ remove ]

  let todoapp k =
    static "section"
    |. str attr "id" "todoapp"
    |- seq
       [ nest (static "header" <.> str attr "id" "header")
         [ static "h1"
           |. text (fun _ _ _ -> "todo")
         ; static "input" <.> str attr "id" "new-todo"
           |. str attr "autofocus" ""
           |. str attr "placeholder" "What needs to be done?"
           |. property "value" (fun _ m _ -> Js.string m.Model.input)
           |. E.input (fun e _ _ ->
               match Js.Opt.to_option e##target with
               | None   -> assert false
               | Some t ->
                 let i = Js.coerce t Dom_html.CoerceTo.input (fun _ -> assert false) in
                 k (Event.ChangeInput (Js.to_string i##value)))
           |. E.keyup (fun e m i ->
               if e##keyCode = 13
                 then k Event.AddInput
                 else ())
         ]
       ; static "section" <.> str attr "id" "main"
         |. seq
            [ static "input"
              |. str attr "id" "toggle-all"
              |. str attr "type" "checkbox"
              |. E.click (fun _ _ _ -> k Event.Toggle)
            ; static "ul"
              |. str attr "id" "todo-list"
              |- items k ]
       ; static "footer" <.> str attr "id" "footer"
         |. seq
            [ static "span" <.> str attr "id" "todo-count"
              |. html (fun _ m _ ->
                  "<strong>" ^ (string_of_int (Model.undone m)) ^ "</strong> items left")
            ; static "ul" <.> str attr "id" "filters"
              |- filters k] ]

  let low_footer =
    let content ="
    <p>Double-click to edit a todo</p>
    <p>Created by <a href='http://computationallyendowed.com'>Spiros Eliopoulos</a></p>
    <p>Part of <a href='http://todomvc.com'>TodoMVC</a></p>
  " in
    static "footer"
    |. str attr "id" "info"
    |. html (fun _ _ _ -> content)

  let make k =
    seq [todoapp k; low_footer]
end

module Storage : sig
  val get : unit -> (string * bool) list
  val set : (string * bool) list -> unit
end = struct
  open Js

  let key = string "todos-ocaml-d3"

  let storage =
    Optdef.case (Dom_html.window##localStorage)
      (fun () -> None)
      (fun s  -> Some s)

  let get () =
    match storage with
    | None   -> []
    | Some s ->
      begin match Opt.to_option (s##getItem(key)) with
      | Some v -> Model.items_of_json (Yojson.Basic.from_string (to_string v))
      | None   -> []
      end

  let set v =
    match storage with
    | None   -> ()
    | Some s ->
      s##setItem (key, string (Yojson.Basic.to_string (Model.items_to_json v)))
end

let main_lazy () =
  (* This code is unused by this example. It's here to demonstrate how to use
   * the code above without lwt, which will improve performance by a bit. *)
  let model = ref { Model.init with Model.items = Storage.get () } in
  let rec go () =
    D3.run ~node:(Dom_html.document##body) (Lazy.force view) !model
  and view = lazy (View.make (fun e ->
    model := Event.handle e !model;
    Storage.set (!model).Model.items;
    go ()))
  in
  go ()
;;

let main_lwt () =
  let stream, push, _ =
    let stream, push = Lwt_stream.create () in
    stream, (fun x -> push (Some x)), (fun () -> push None)
  in
  let view = View.make push in
  let init = { Model.init with Model.items = Storage.get () } in
  let node = (Dom_html.document##body) in
  D3.run ~node view init;
  Lwt_stream.fold (fun e m ->
    let m' = Event.handle e m in
    Storage.set m'.Model.items;
    D3.run ~node view m';
    m')
  stream init
;;

let _ =
  Lwt_js_events.async main_lwt
