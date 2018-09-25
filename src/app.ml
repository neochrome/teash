type ('args, 'model, 'msg) config = {
	init : 'args -> 'model * 'msg Cmd.t;
	update : 'model -> 'msg -> 'model * 'msg Cmd.t;
	view :  'model -> Notty.image;
	subscriptions : 'model -> 'msg Sub.t;
	shutdown : 'model -> unit;
}

let run : ('args, 'model, 'msg) config -> 'args -> unit =
	fun { init; update; view; subscriptions; shutdown; } args ->

		let term = Notty_lwt.Term.create () in
		let (msgs : 'msg Lwt_stream.t), (push_msg : 'msg option -> unit) = Lwt_stream.create () in
		let sub_context = Sub.({ push = push_msg; term; }) in

		let init_model,init_cmds = init args in
		let init_subs = subscriptions init_model |> Sub.(update sub_context empty) in

		let rec process subs cmds model =
			let%lwt () = model |> view |> Notty_lwt.Term.image term in
			let () = Lwt.async (fun () -> Cmd.run cmds push_msg) in
			try%lwt
				let%lwt msg = Lwt_stream.last_new msgs in
				let new_model,new_cmds = update model msg in
				let new_subs = subscriptions new_model |> Sub.(update sub_context subs) in
				(process [@tailcall]) new_subs new_cmds new_model
			with Lwt_stream.Empty -> begin
				shutdown model;
				Lwt.return ()
			end
		in
		Lwt_main.run (process init_subs init_cmds init_model)

let exit = Cmd.call Lwt.return_none
