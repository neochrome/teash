open Teash

type msg =
	| Tick of Time.time
	| Key of Notty.Unescape.key

let init () = Time.now (), Cmd.none

let update time = function
	| Tick time' -> time', Cmd.none
	| Key (`Escape, _mods) -> time, App.exit
	| Key _ -> time, Cmd.none

let view time = Notty.(
	let tm = Unix.localtime time in
	I.(
		strf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec
		<->
		string A.empty "press ESC to quit"
	)
)

let subscriptions _model =
	Sub.batch [
		Time.(every second) (fun time -> Tick time);
		Keyboard.presses (fun key -> Key key);
	]

let () =
	App.run {
		init;
		update;
		view;
		subscriptions;
		shutdown = (fun _model -> ());
	} ()
