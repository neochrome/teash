type 'msg t =
	| None
	| Batch of 'msg t list
	| Call of 'msg option Lwt.t

let none = None
let batch cmds = Batch cmds
let call call = Call call
let msg msg = call (Lwt.return_some msg)

let run : 'msg t -> ('msg option -> unit) -> unit Lwt.t =
	fun cmd push ->
		let rec flatten : 'msg t -> unit Lwt.t list =
			function
			| None -> []
			| Batch cmds -> cmds |> List.map flatten |> List.concat
			| Call call -> [Lwt.(call >|= push)]
		in cmd |> flatten |> Lwt.choose
