open Teash

type msg =
	| Inc
	| Dec
	| Set of int
	| Reset
	| Key of Notty.Unescape.key

let init () = 0, Cmd.none

let key_to_cmd = function
	| (`Arrow `Up), _mods -> Cmd.msg Inc
	| (`Arrow `Down), _mods -> Cmd.msg Dec
	| (`ASCII 's'), _mods -> Cmd.msg (Set 42)
	| (`ASCII 'r'), _mods -> Cmd.msg Reset
	| (`ASCII 'q'), _mods -> App.exit
	| _ -> Cmd.none

let update counter = function
	| Inc -> counter + 1, Cmd.none
	| Dec -> counter - 1, Cmd.none
	| Set n -> n, Cmd.none
	| Reset -> 0, Cmd.none
	| Key key -> counter, (key_to_cmd key)

let view counter = Notty.(
	[
		I.strf "counter: %d" counter;
		I.string A.empty "up - inc, down - dec, s - set to 42, r - reset, q - quit";
	] |> I.vcat
)

let subscriptions _model =
	Keyboard.presses (fun key -> Key key)

let () =
	App.run {
		init;
		update;
		view;
		subscriptions;
		shutdown = (fun _model -> ());
	} ()
