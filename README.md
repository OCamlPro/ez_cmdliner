ezcmd : Easy Cmdliner
=====================

`Cmdliner` is a very nice library to manage command line options for
OCaml programs. It has a powerful way of displaying help through
pagers and manpages. However, it uses a complex set of combinators to
declare these options, and many people still prefer the more
structured way to declare options of the Stdlib `Arg` module.

`Ezcmd` is a simple layer over `Cmdliner` that provides an interface
very similar to the `Arg` module, while still benefiting from
`Cmdliner` nice features.

Some differences with `Arg`:
* All arguments are parsed first, then the functions are called. It is not
 possible to recover the order of the arguments (except within anonymous
 arguments), or to modify the list of arguments (for example if a plugin is
 loaded);
* `Arg.Tuple` and `Arg.Rest` are not available in `Ezcmd`.

Building
--------

To build and install:
```
ocp-build init
ocp-build make
ocp-build install
```

Basic Usage
-----------

`Ezcmd` provides a simple compatibility layer with `Arg`. It re-implements
the most common constructors of `Arg`:

```
open Ezcmd.Modules

...

let () =
  Arg.parse ~name:"test"
  [ "--toto", Arg.Set toto_ref, "Set the toto_ref reference" ]
  (fun s -> print_endline s)
  "Simple test example"
```

Here, `Arg.Set` and `Arg.parse` are actually defined in the module
`Ezcmd.Modules.Arg`.

Simple Usage
------------

Here is a more advanced example:

```
open Ezcmd.Modules

let () =
  let version = "1.2.3" in
  
  let cmd_name = "my-tool" in
  let cmd_doc = "A generic tool" in
  let cmd_man = [ `P "Some documentation on my-tool" ] in
  let cmd_args = [
     ["f", "file"],
     Arg.String (fun file -> ...),
     Ezcmd.info
        ~docs:"GENERIC OPTIONS"
        ~docv:"FILE"
        "Print information on $(docv)";

     [],
     Arg.Anons (fun dir -> ...),
     Ezcmd.info
        ~docv:"DIR"
        "Directories on which to work";
     ]
  in
  let cmd_action () = ... in
  let cmd = {
    Arg.cmd_name;
        cmd_doc; cmd_man; cmd_args; cmd_action
  }
  in
  Ezcmd.main ~version cmd
;;
```

Sub-command Usage
-----------------

It is possible to define a command with one level of sub-commands
using `Ezcmd.main_with_subcommands`. See several examples in `tests/`.


Documentation and Examples
--------------------------

See the `ezcmd.mli` file for the full (but short) interface and
the `tests/` directory for a few examples.

