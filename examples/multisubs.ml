open Teash

type msg =
	| Key of Notty.Unescape.key
	| A | B

type model = {
	a : int;
	b : int;
}

let init () = {
	a = 0;
	b = 0;
}, Cmd.none


let update model = function
	| Key (`Escape, _mods) -> model, App.exit
	| A -> { model with a = model.a + 1 }, Cmd.none
	| B -> { model with b = model.b + 1 }, Cmd.none
	| _ -> model, Cmd.none

let view model = Notty.([
	I.string A.(fg blue) "ESC - quit";
	I.strf "a: %d, b: %d" model.a model.b;
] |> I.vcat)

let subscriptions _model =
	Sub.batch [
		Keyboard.presses (fun key -> Key key);
		Time.(every second (fun _ -> A));
		Time.(every second (fun _ -> B));
	]

let () =
	App.run {
		init;
		update;
		view;
		subscriptions;
		shutdown = (fun _model -> ());
	} ()
