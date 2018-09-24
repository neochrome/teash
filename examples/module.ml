open Teash

module Module = struct
	type msg = Tick | Tock | Key of Notty.Unescape.key
	type model = { state : string; n : int }
	let initial = { state = ""; n = 0; }
	let subscriptions _model = Keyboard.presses (fun k -> Key k)
	let trigger model = model, Cmd.msg Tick
	let update model = function
		| Tick -> { model with state = "tick"; n = model.n + 1; }, Time.(delay second Tock)
		| Tock -> { model with state = "tock"; n = model.n + 1; }, Time.(delay second Tick)
		| Key (`ASCII 'r', _mods) -> initial, Cmd.none
		| Key (`Escape, _mods) -> model, App.exit
		| _ -> model, Cmd.none
	let view model = Notty.(I.strf "counter: %d, %s" model.n model.state)
end

type msg =
	| Key of Notty.Unescape.key
	| ModuleMsg of Module.msg

let module_msg m = ModuleMsg m

type model = {
	dispatched : int;
	state : Module.model;
}

let init () = {
	dispatched = 0;
	state = Module.initial;
}, Cmd.none

let update model = function
	| Key (`Escape, _mods) -> model, App.exit
	| Key (`ASCII 't', _mods) ->
		let state,cmd = Module.trigger model.state in
		{ model with state }, Cmd.map module_msg cmd
	| ModuleMsg msg ->
		let dispatched = model.dispatched + 1 in
		let state,cmd = Module.update model.state msg in
		{ model with dispatched; state }, Cmd.map module_msg cmd
	| _ -> model, Cmd.none

let view model = Notty.([
	I.string A.(fg blue) "t - trigger loop, r - reset counter, ESC - quit";
	I.strf "dispatched: %d" model.dispatched;
	I.string A.empty "module state:";
	Module.view model.state;
] |> I.vcat)

let subscriptions model =
	Sub.batch [
		Keyboard.presses (fun key -> Key key);
		Module.subscriptions model.state |> Sub.map module_msg;
	]

let () =
	App.run {
		init;
		update;
		view;
		subscriptions;
		shutdown = (fun _model -> ());
	} ()
