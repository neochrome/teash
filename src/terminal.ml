type size = { width : int; height : int; }

let resizes : (size -> 'msg) -> 'msg Sub.t =
	fun tagger ->
		Sub.registration "terminal:resizes" (fun { push; term; } ->
			Notty_lwt.Term.events term
			|> Lwt_stream.filter_map
				(function
				| `Resize (width,height) -> Some { width; height; }
				| _ -> None
				)
			|> Lwt_stream.map tagger
			|> Lwt_stream.iter (fun msg -> push (Some msg))
		) tagger
