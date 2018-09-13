type position = { x : int; y : int; }

let downs : (Notty.Unescape.button -> position -> Notty.Unescape.mods -> 'msg) -> 'msg Sub.t =
	fun tagger ->
		Sub.registration "mouse:downs" (fun { push; term; } ->
			Notty_lwt.Term.events term
			|> Lwt_stream.filter_map
				(function
				| `Mouse (`Press `Scroll _, _, _) -> None
				| `Mouse (`Press button, (x,y), mods) -> Some (button, {x;y},mods)
				| _ -> None
				)
			|> Lwt_stream.map (fun (button,pos,mods) -> tagger button pos mods)
			|> Lwt_stream.iter (fun msg -> push (Some msg))
		)

let ups : (position -> Notty.Unescape.mods -> 'msg) -> 'msg Sub.t =
	fun tagger ->
		Sub.registration "mouse:ups" (fun { push; term; } ->
			Notty_lwt.Term.events term
			|> Lwt_stream.filter_map
				(function
				| `Mouse (`Release, (x,y), mods) -> Some ({x;y},mods)
				| _ -> None
				)
			|> Lwt_stream.map (fun (pos,mods) -> tagger pos mods)
			|> Lwt_stream.iter (fun msg -> push (Some msg))
		)

let drags : (position -> Notty.Unescape.mods -> 'msg) -> 'msg Sub.t =
	fun tagger ->
		Sub.registration "mouse:drags" (fun { push; term; } ->
			Notty_lwt.Term.events term
			|> Lwt_stream.filter_map
				(function
				| `Mouse (`Drag, (x,y), mods) -> Some ({x;y},mods)
				| _ -> None
				)
			|> Lwt_stream.map (fun (pos,mods) -> tagger pos mods)
			|> Lwt_stream.iter (fun msg -> push (Some msg))
		)

let scrolls : ([`Up | `Down] -> position -> Notty.Unescape.mods -> 'msg) -> 'msg Sub.t =
	fun tagger ->
		Sub.registration "mouse:scrolls" (fun { push; term; } ->
			Notty_lwt.Term.events term
			|> Lwt_stream.filter_map
				(function
				| `Mouse (`Press (`Scroll dir),(x,y),mods) -> Some (dir,{x;y;},mods)
				| _ -> None
				)
			|> Lwt_stream.map (fun (dir,pos,mods) -> tagger dir pos mods)
			|> Lwt_stream.iter (fun msg -> push (Some msg))
		)

(* for low-level use *)
let events : (Notty.Unescape.mouse -> 'msg) -> 'msg Sub.t =
	fun tagger ->
		Sub.registration "mouse:events" (fun { push; term; } ->
			Notty_lwt.Term.events term
			|> Lwt_stream.filter_map
				(function
				| `Mouse event -> Some event
				| _ -> None
				)
			|> Lwt_stream.map tagger
			|> Lwt_stream.iter (fun msg -> push (Some msg))
		)
