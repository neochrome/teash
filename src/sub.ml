open Operators

type 'msg context = {
	push : 'msg option -> unit;
	term : Notty_lwt.Term.t;
}
type 'msg init = 'msg context -> unit Lwt.t

type 'msg t =
	| None
	| Batch of 'msg t list
	| Registration of string * 'msg init

let none = None
let batch subs = Batch subs
let registration key (init : 'msg init) tagger =
	let ref = Obj.magic tagger |> string_of_int in
	Registration (key ^ ref, init)

module M = Misc.StringMap

let empty : unit Lwt.t M.t = M.empty

let rec flatten = function
	| None -> []
	| Registration (key,init) -> [key,init]
	| Batch batch -> batch |> List.map flatten |> List.concat

let update (context : 'msg context) (active : unit Lwt.t M.t) (subs : 'msg t) =
	let collect = List.fold_left (fun regs (key,init) -> M.add key init regs) M.empty in
	let activate init = let sub = init context in Lwt.on_cancel sub (fun () -> ()); sub in
	let deactivate = Lwt.cancel in
	let inactive = subs |> flatten |> collect in
	M.merge (fun _key i a ->
		match i, a with
		| Some i, None -> Some (activate i)
		| None, Some a -> deactivate a; None
		| _ -> a
	) inactive active

let map : ('a -> 'b) -> 'a t -> 'b t =
	fun mapper sub ->
		sub
		|> flatten
		|> List.map (fun (key,init) -> key, (fun ctx ->
				init { ctx with push = Option.map mapper >> ctx.push }
		)) |> List.map (fun (key,init) -> Registration (key,init))
		|> batch
