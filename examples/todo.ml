module Model = struct
  type filter =
    | All
    | Active
    | Completed

  let string_of_filter : filter -> string = function
    | All -> "All"
    | Active -> "Active"
    | Completed -> "Completed"

  type t =
    { input   : string
    ; items   : (string * bool) list
    ; filter  : filter
    ; editing : (string * int) option
    ; toggle  : bool
    }

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

module Event = struct
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

  (* This is an event handler. It specifies how the model should be modified
   * based on a high-level Event. Note that becuase events are represented as a
   * variant of an algebraic data type, the type system requires you to put
   * event-handling code all in one place. This will serve as the body of the
   * application's event loop. *)
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

  let (stream : t Lwt_stream.t), push, complete =
    let stream, push = Lwt_stream.create () in
    stream, (fun x -> push (Some x)), (fun () -> push None)
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

  let items =
    selectAll "li"
    |. data (fun m _ -> items_of_model m)
    |- nest enter
       [ append "li"
         |. E.dblclick (fun _ _ i -> Event.(push (Edit i))) ]
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
                    then Event.(push (Uncheck i))
                    else Event.(push (Check   i)))
            ; static "label"
              |. text (fun _ m _ -> m.text)
            ; static "button"
              |. str attr "class" "destroy"
              |. E.click (fun _ _ i -> Event.(push (Delete i))) ]
          ; static "input"
            |. str attr "class" "edit"
            |. property "value" (fun _ m i -> Js.string m.text)
            |. E.input (fun e _ _ ->
                match Js.Opt.to_option e##target with
                | None   -> assert false
                | Some t ->
                  let i = Js.coerce t Dom_html.CoerceTo.input (fun _ -> assert false) in
                  Event.(push (ChangeEdit (Js.to_string i##value))))
            |. E.keyup (fun e _ _ ->
                 match e##keyCode with
                 | 27 -> Event.(push CancelEdit) (* ESC_KEY   *)
                 | 13 -> Event.(push CommitEdit) (* ENTER_KEY *)
                 | _  -> ())
            |. E.blur (fun _ _ _ -> Event.(push CancelEdit)) ]
    |- chain
       [ exit <.> remove ]

  let filters =
    selectAll "li"
    |. data (fun m _ ->
        let open Model in
        [ (All      , m.filter = All      , "#/")
        ; (Active   , m.filter = Active   , "#/active")
        ; (Completed, m.filter = Completed, "#/completed")
        ])
    |- nest enter
       [ append "li"
         |. append "a"
         |. attr "href" (fun _ (_, _, href) _ -> href)
         |. text (fun _ (f, _, _) _ -> Model.string_of_filter f) ]
    |- nest update
       [ select "a"
         |. classed "selected" (fun _ (_, active, _) _ -> active) ]
         |. E.click (fun _ (f,_,_) _ -> Event.(push (Filter f)))
    |- nest exit
       [ remove ]

  let todoapp =
    static "section"
    |. str attr "id" "todoapp"
    |- seq
       [ static "header" <.> str attr "id" "header"
         |. static "h1"
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
               Event.(push (ChangeInput (Js.to_string i##value))))
         |. E.keyup (fun e m i ->
             if e##keyCode = 13
               then Event.(push AddInput)
               else ())
       ; static "section" <.> str attr "id" "main"
         |. seq
            [ static "input"
              |. str attr "id" "toggle-all"
              |. str attr "type" "checkbox"
              |. E.click (fun _ _ _ -> Event.(push Toggle))
            ; static "ul"
              |. str attr "id" "todo-list"
              |- items ]
       ; static "footer" <.> str attr "id" "footer"
         |. seq
            [ static "span" <.> str attr "id" "todo-count"
              |. html (fun _ m _ ->
                  "<strong>" ^ (string_of_int (Model.undone m)) ^ "</strong> items left")
            ; static "ul" <.> str attr "id" "filters"
              |- filters ] ]

  let low_footer =
    let content ="
    <p>Double-click to edit a todo</p>
    <p>Created by <a href='http://computationallyendowed.com'>Spiros Eliopoulos</a></p>
    <p>Part of <a href='http://todomvc.com'>TodoMVC</a></p>
  " in
    static "footer"
    |. str attr "id" "info"
    |. html (fun _ _ _ -> content)

  let render selection model =
    run selection model (seq [todoapp; low_footer])
end

let main () =
  View.render "body" Model.init;
  Lwt_stream.fold (fun e m ->
    let m' = Event.handle e m in
    View.render "body" m';
    m')
  Event.stream Model.init

let _ =
  Lwt_js_events.async main
