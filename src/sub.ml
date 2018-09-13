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
let registration key (init : 'msg init) = Registration (key, init)

let empty : unit Lwt.t Misc.StringMap.t = Misc.StringMap.empty

let update (context : 'msg context) (active : unit Lwt.t Misc.StringMap.t) (subs : 'msg t) =
	let module M = Misc.StringMap in
	let rec flatten (registrations : 'msg init M.t) = function
		| None -> registrations
		| Batch batch -> batch |> List.fold_left flatten registrations
		| Registration (key,init) -> registrations |> M.add key init
	in
	let activate init = let sub = init context in Lwt.on_cancel sub (fun () -> ()); sub in
	let deactivate sub = Lwt.cancel sub in
	let inactive = subs |> flatten M.empty in
	M.merge (fun _key i a ->
		match i, a with
		| Some i, None -> Some (activate i)
		| None, Some a -> deactivate a; None
		| _ -> a
	) inactive active
