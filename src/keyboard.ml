let presses : (Notty.Unescape.key -> 'msg) -> 'msg Sub.t =
	fun tagger ->
		Sub.registration "keyboard:presses" (fun { push; term; } ->
			Notty_lwt.Term.events term
			|> Lwt_stream.filter_map
				(function
				| `Key key -> Some key
				| _ -> None
				)
			|> Lwt_stream.map tagger
			|> Lwt_stream.iter (fun msg -> push (Some msg))
		) tagger
