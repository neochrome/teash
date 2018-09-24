type 'msg t =
	| None
	| Batch of 'msg t list
	| Call of 'msg option Lwt.t

let none = None
let batch cmds = Batch cmds
let call call = Call call
let msg msg = call (Lwt.return_some msg)

let rec flatten = function
	| None -> []
	| Call call -> [call]
	| Batch cmds -> cmds |> List.map flatten |> List.concat

let map : ('a -> 'b) -> 'a t -> 'b t =
	fun mapper cmd ->
		cmd
		|> flatten
		|> List.map (Lwt.map (Option.map mapper))
		|> List.map (fun call -> Call call)
		|> batch

let run : 'msg t -> ('msg option -> unit) -> unit Lwt.t =
	fun cmd push ->
		cmd
		|> flatten
		|> List.map (Lwt.map push)
		|> Lwt.choose
