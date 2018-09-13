type time = float

let now : unit -> time = Unix.time

let every : time -> (time -> 'msg) -> 'msg Sub.t =
	fun interval tagger ->
		let key = "every:" ^ (string_of_float interval) in
		Sub.registration key (fun { push; _ } ->
			let rec repeat () =
				tagger (now ()) |> fun msg -> push (Some msg);
				let%lwt () = Lwt_unix.sleep interval in
				repeat ()
			in repeat ()
		)

let delay : time -> 'msg -> 'msg Cmd.t =
	fun delay msg ->
		Lwt.(
			Lwt_unix.sleep delay
			>|= fun () -> Some msg
		) |> Cmd.call

let millisecond : time = 1.0 /. 1000.0
let second : time = 1.0
let minute : time = second *. 60.0
let hour : time = minute *. 60.0
let day : time = hour *. 24.0

let in_milliseconds (t : time) = t /. 1000.0
let in_seconds (t: time) = t
let in_minutes (t: time) = t /. minute
let in_hours (t: time) = t /. hour
let in_days (t: time) = t /. day
