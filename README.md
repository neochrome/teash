# TEASH
[TEA][] for the shell, in OCaml.

## Description
Teash is an interpretation of [TEA][] for the shell,
using [Lwt][] and [Notty][] to gain async and terminal
rendering capabilities. Use it to build interactive
terminal programs that are mostly event driven and
organized according to a `Model -> Update -> View` pattern.
Then compile them to either bytecode or native binaries.

Most of the inspiration for this library comes from the
excellent [bucklescript-tea][] project.

## Installation
`opam install teash`

## Getting started
Make sure to add a reference to the `teash` library to your build.

A simple *Hello world* program might look like this:
```ocaml
(* bring Teash into scope *)
open Teash

(* a type to define possible messages/events in the program *)
type msg =
	| Key of Notty.Unescape.key (* this will hold key presses *)

(* initialize the model *)
let init () = ()

(* the central message/event handler *)
let update model = function
	| Key (`Escape, _mods) -> model, App.exit (* listen for ESC and exit *)
	| Key _ -> model, Cmd.none

(* the view is just a function that returns a Notty.image given the model *)
let view _model =
	Notty.(I.string A.(fg red) "Hello World!")

(* hookup subscription to events *)
let subscriptions _model =
	Keyboard.presses (fun key -> Key key) (* subscribe to key presses and map to our msg type *)

(* "main" *)
let () =
	App.run {
		init;
		update;
		view;
		subscriptions;
		shutdown = (fun _model -> ());
	} ()

```

For further details have a look at the [examples](/examples).


[TEA]: https://guide.elm-lang.org/architecture/ "The Elm Architecture"
[Lwt]: https://github.com/ocsigen/lwt
[Notty]: https://github.com/pqwy/notty
[bucklescript-tea]: https://github.com/OvermindDL1/bucklescript-tea

[api]: https://neochrome.github.io/teash/
