(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Ez_subst.V1

module EZCMD = struct

  module TYPES = struct
    type block =
      [ `S of string     (* section title *)
      | `P of string     (* paragraph *)
      | `Pre of string   (* code *)
      | `I of string * string (* item (description, info) *)
      | `Noblank
      | `Blocks of block list ]

    type env = {
      env_docs : string option ;
      env_doc : string option ;
      env_var : string ;
    }

    type info = {
      arg_docs : string option ;
      arg_docv : string option ;
      arg_env : env option ;
      arg_version : string option ;
      arg_doc : string ;
    }

    module Arg = struct
      type spec =
        (* Same as Arg. But they should only appear at most once on the
           command-line, or Cmdliner will complain. *)
        | Unit of (unit -> unit)
        | Bool of (bool -> unit)
        | Set_bool of bool ref
        | Set of bool ref
        | Clear of bool ref
        | String of (string -> unit)
        | Set_string of string ref
        | Int of (int -> unit)
        | Set_int of int ref
        | Float of (float -> unit)
        | Set_float of float ref
        | Symbol of string list * (string -> unit)
        | File of (string -> unit)
        (* Anonymous arguments. `Anon(n,f)` means the anonymous argument
           at position `n`. `Anons f` means all the anonymous arguments. *)
        | Anon of int * (string -> unit)
        | Anons of (string list -> unit)
    end

    type arg_list = (string list * Arg.spec * info) list

    type sub = {
      sub_name : string ;
      sub_action : unit -> unit;
      sub_args : arg_list;
      sub_man : block list;
      sub_version : string option ;
      sub_doc : string;
    }

    type command = {
      cmd_name : string;
      cmd_action : unit -> unit;
      cmd_args : arg_list;
      cmd_man : Cmdliner.Manpage.block list;
      cmd_doc : string;
    }

  end

  include TYPES.Arg
  module RAWTYPES = TYPES

  let raw_env x = x
  let raw_sub x = x
  let raw_info x = x

  open TYPES
  open TYPES.Arg
  open Cmdliner

  let info ?docs ?docv ?env ?version doc =
    {
      arg_docs = docs ;
      arg_docv = docv ;
      arg_env = env ;
      arg_version = version ;
      arg_doc = doc ;
    }

  let env ?docs ?doc env_var =
    { env_docs = docs ;
      env_doc = doc ;
      env_var }

  let rec term_of_list list =
    match list with
    | [] -> Term.(const ())
    | (args, action, info) :: tail -> (
        let x = term_of_list tail in
        let arg_info =
          let env = match info.arg_env with
            | None -> None
            | Some env -> Some (
                Term.env_info
                  ?docs:env.env_docs
                  ?doc:env.env_doc
                  env.env_var) in
          Cmdliner.Arg.info
            ?docs:info.arg_docs
            ?docv:info.arg_docv
            ?env
            ~doc:info.arg_doc
            args
        in
        match action with
        | Unit f ->
            let term = Arg.(value & flag_all & arg_info) in
            let f () x = List.iter (fun x -> if x then f ()) x in
            Term.(const f $ x $ term)
        | Bool f ->
            let term = Arg.(value & flag_all & arg_info) in
            let f () x = List.iter f x in
            Term.(const f $ x $ term)
        | Set_bool r ->
            let term = Arg.(value & opt (some bool) None & arg_info) in
            let f () = function None -> () | Some s -> r := s in
            Term.(const f $ x $ term)
        | Set r ->
            let term = Arg.(value & flag & arg_info) in
            let f () x = if x then r := true in
            Term.(const f $ x $ term)
        | Clear r ->
            let term = Arg.(value & flag & arg_info) in
            let f () x = if x then r := false in
            Term.(const f $ x $ term)
        | String f ->
            let term = Arg.(value & opt_all string [] & arg_info) in
            let f () x = List.iter f x in
            Term.(const f $ x $ term)
        | Set_string r ->
            let term = Arg.(value & opt (some string) None & arg_info) in
            let f () = function None -> () | Some s -> r := s in
            Term.(const f $ x $ term)
        | Int f ->
            let term = Arg.(value & opt (some int) None & arg_info) in
            let f () = function None -> () | Some s -> f s in
            Term.(const f $ x $ term)
        | Set_int r ->
            let term = Arg.(value & opt (some int) None & arg_info) in
            let f () = function None -> () | Some s -> r := s in
            Term.(const f $ x $ term)
        | Float f ->
            let term = Arg.(value & opt (some float) None & arg_info) in
            let f () = function None -> () | Some s -> f s in
            Term.(const f $ x $ term)
        | Set_float r ->
            let term = Arg.(value & opt (some float) None & arg_info) in
            let f () = function None -> () | Some s -> r := s in
            Term.(const f $ x $ term)
        | Symbol (symbols, f) ->
            let symbol = Arg.enum (List.map (fun s -> (s, s)) symbols) in
            let term = Arg.(value & opt (some symbol) None & arg_info) in
            let f () = function None -> () | Some s -> f s in
            Term.(const f $ x $ term)
        | File f ->
            let term = Arg.(value & opt_all file [] & arg_info) in
            let f () x = List.iter f x in
            Term.(const f $ x $ term)
        | Anon (n, f) ->
            let term = Arg.(value & pos n (some string) None & arg_info) in
            let f () = function None -> () | Some s -> f s in
            Term.(const f $ x $ term)
        | Anons f ->
            let term = Arg.(value & pos_all string [] & arg_info) in
            let f () x = f x in
            Term.(const f $ x $ term) )

  let cmd_exits = Term.default_exits

  let sub sub_name ~doc ?(args = []) ?(man = []) ?version sub_action =
    { sub_name ;
      sub_doc = doc ;
      sub_action ;
      sub_args = args ;
      sub_man = man ;
      sub_version = version ;
    }

  let create_sub ?version sub ~common_args =
    let man = sub.sub_man in
    let exits = cmd_exits in
    let doc = sub.sub_doc in
    let args = common_args @ sub.sub_args in
    ( Term.(const sub.sub_action $ term_of_list args),
      Term.info sub.sub_name ?version ~doc ~sdocs:Manpage.s_common_options ~exits
        ~man )

  let help more_topics man_format cmds topic =
    match topic with
    | None -> `Help (`Pager, None) (* help about the program. *)
    | Some topic -> (
        let topics = ("topics" :: List.map fst more_topics) @ cmds in
        let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e -> `Error (false, e)
        | `Ok t when t = "topics" ->
            List.iter print_endline topics;
            `Ok ()
        | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
        | `Ok t ->
            let page =
              ((topic, 7, "", "", ""), `S topic :: List.assoc t more_topics)
            in
            `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page) )

  let help_cmd ~name ~man ~topics =
    let topic =
      let doc = "The topic to get help on. `topics' lists the topics." in
      Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
    in
    let doc = Printf.sprintf "display help about %s and %s commands" name name in
    let man =
      [
        `S Manpage.s_description;
        `P "Prints help about darcs commands and other subjects...";
        `Blocks man;
      ]
    in
    ( Term.(ret (const (help topics) $ Arg.man_format $ Term.choice_names $ topic)),
      Term.info "help" ~doc ~exits:Term.default_exits ~man )

  let default_cmd ~name ?version ~doc ~man =
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    ( Term.(ret (const (`Help (`Pager, None)))),
      Term.info name ?version ~doc ~sdocs ~exits ~man )

  let main_with_subcommands ~name ?version ?default ~doc ?(man=[]) ?(topics = []) ?(common_args=[]) ?argv subs
    =
    let cmds = List.map (create_sub ?version ~common_args) subs in
    let default_cmd =
      match default with
      | None -> default_cmd ~name ?version ~doc ~man
      | Some cmd -> create_sub ?version cmd ~common_args
    in
    let cmds =
      if List.exists (fun cmd -> cmd.sub_name = "help") subs then cmds
      else cmds @ [ help_cmd ~name ~man ~topics ]
    in
    match Term.eval_choice ~catch:false default_cmd ?argv cmds with
    | `Ok () -> ()
    | t -> Term.exit t

  let main ?version ?argv cmd =
    let cmd = create_sub ?version cmd ~common_args:[] in
    match Term.eval ~catch:false ?argv cmd with
    | `Ok () -> ()
    | `Error `Parse -> print_endline "toto"
    | t -> Term.exit t

  module MANPAGE = Cmdliner.Manpage

  let translate ?docs arg_list =
    List.map
      (fun (arg, spec, doc) ->
         let len = String.length arg in
         let arg =
           if len > 0 && arg.[0] = '-' then
             if len > 1 && arg.[1] = '-' then String.sub arg 2 (len - 2)
             else String.sub arg 1 (len - 1)
           else arg
         in
         ([ arg ], spec, info ?docs doc))
      arg_list

  let translate_anon arg_anon =
    [
      ([], Anons (fun list -> List.iter arg_anon list), info "General arguments");
    ]

  let parse ?name ?version ?(man = []) arg_list arg_anon arg_usage =
    let sub_args = translate arg_list @ translate_anon arg_anon in
    let sub_name =
      match name with None -> "COMMAND" | Some sub_name -> sub_name
    in
    let cmd =
      {
        sub_name;
        sub_doc = arg_usage;
        sub_args;
        sub_man = man;
        sub_version = None;
        sub_action = (fun () -> ());
      }
    in
    main ?version cmd

  module RST = struct

    open TYPES

    let doclang_to_rst ?(map= StringMap.empty) s =
      let paren map s =
        match StringMap.find s map with
        | s -> s
        | exception Not_found ->
            match EzString.chop_prefix s ~prefix:"b," with
            | Some s -> Printf.sprintf "**%s**" s
            | None ->
                match EzString.chop_prefix s ~prefix:"i," with
                | Some s -> Printf.sprintf "*%s*" s
                | None ->
                    s
      in
      EZ_SUBST.string ~paren:paren ~ctxt:map s

    let man_to_rst ?(map = StringMap.empty) ( man : block list ) =
      let b = Buffer.create 1000 in
      let rec iter = function
        | `S s ->
            let s = doclang_to_rst ~map s in
            Printf.bprintf b "\n\n**%s**\n\n" s
        | `Blocks list -> List.iter iter list
        | `I (label, txt) ->
            Printf.bprintf b "\n* %s\n  %s\n"
              ( doclang_to_rst ~map label )
              ( doclang_to_rst ~map txt )
        | `Noblank -> ()
        | `P par ->
            Printf.bprintf b "\n%s\n" ( doclang_to_rst ~map par )
        | `Pre code ->
            let code = doclang_to_rst ~map code in
            Printf.bprintf b "::\n\n  %s\n\n"
              ( String.concat "\n  "
                  ( EzString.split code '\n' ))

      in
      List.iter iter man;
      Buffer.contents b


    let arg_name info name =
      match info.arg_docv with
      | None -> name
      | Some name -> name

    let arg_name f info =
      match f with
      | Arg.String _ -> arg_name info "STRING"
      | Arg.Bool _ -> arg_name info "BOOL"
      | Arg.Int _ -> arg_name info "INT"
      | Arg.Float _ -> arg_name info "FLOAT"
      | Arg.Set_string _ -> arg_name info "STRING"
      | Arg.Set_bool _ -> arg_name info "BOOL"
      | Arg.Set_int _ -> arg_name info "INT"
      | Arg.Set_float _ -> arg_name info "FLOAT"
      | Arg.Unit _
      | Arg.Set _
      | Arg.Clear _
        -> ""
      | Arg.File _ -> arg_name info "FILE"
      | Arg.Anon (_, _) -> arg_name info "ARGUMENT"
      | Arg.Anons _ -> arg_name info "ARGUMENTS"
      | Arg.Symbol (list, _) ->
          arg_name info (Printf.sprintf "[%s]"
                           ( String.concat "|" list))


    let print_options b options =

      List.iter (fun (option, f, info)  ->
          let arg_name = arg_name f info in
          let map = StringMap.add "docv" arg_name StringMap.empty in
          Printf.bprintf b "\n* %s "
            (match option with
             | [] ->
                 Printf.sprintf ":code:`%s`" arg_name
             | _ ->
                 let arg_name = if arg_name = "" then "" else
                     " " ^ arg_name in
                 String.concat " or " @@
                 List.map (fun s ->
                     if String.length s = 1 then
                       Printf.sprintf ":code:`-%s%s`" s arg_name
                     else
                       Printf.sprintf ":code:`--%s%s`" s arg_name
                   ) option );
          Printf.bprintf b "  %s%s\n"
            (match info.arg_version with
             | None -> ""
             | Some version -> Printf.sprintf "(since version %s) " version)
            ( doclang_to_rst ~map info.arg_doc )

        ) options

    let to_rst ?(name=Filename.basename Sys.argv.(0)) commands common_args =

      let commands = List.map raw_sub commands in

      let commands = List.sort (fun cmd1 cmd2 ->
          compare cmd1.sub_name cmd2.sub_name) commands in
      let b = Buffer.create 10000 in

      Printf.bprintf b
        {|
Sub-commands and Arguments
==========================
|};

      begin match common_args with
        | [] -> ()
        | _ ->
            Printf.bprintf b "Common arguments to all sub-commands:\n\n";
            print_options b ( List.sort compare common_args );
      end;

      Printf.bprintf b {|
Overview of sub-commands::
|};

      List.iter (fun cmd ->
          Printf.bprintf b "  \n  %s%s\n    %s\n" cmd.sub_name
            (match cmd.sub_version with
             | None -> ""
             | Some version -> Printf.sprintf " (since version %s)" version)
            (doclang_to_rst cmd.sub_doc)
        ) commands;

      List.iter (fun cmd ->

          let s = Printf.sprintf "\n\n%s %s%s" name cmd.sub_name
              (match cmd.sub_version with
               | None -> ""
               | Some version -> Printf.sprintf " (since version %s)" version)
          in
          Printf.bprintf b "%s\n%s\n\n" s
            ( String.make ( String.length s) '~' );

          Printf.bprintf b "%s\n\n"
            (doclang_to_rst cmd.sub_doc);

          Printf.bprintf b "%s" (man_to_rst cmd.sub_man);

          let options = cmd.sub_args in
          (* TODO: compare may fail on arguments because they contain closures... *)
          let options = List.sort compare options in
          let options = List.map (fun (args, f, info) ->
              (args, f, raw_info info)) options in


          Printf.bprintf b "\n**USAGE**\n::\n  \n  %s %s%s [OPTIONS]\n\n"
            name
            cmd.sub_name
            ( String.concat ""
                ( List.map (function
                        ( [], f, info ) ->
                          " " ^ arg_name f info
                      | _ -> "") options))
          ;
          Printf.bprintf b "Where options are:\n\n";
          print_options b options;

        ) commands;

      Buffer.contents b

  end

  let to_rst = RST.to_rst

  module MAKE( M : sig

      val command : string
      val version : string
      val about : string
      val usage : string

      (* If this env variable is set, a backtrace will be generated on errors *)
      val backtrace_var : string option

      (* manipulate verbosity *)
      val get_verbosity : unit -> int
      val set_verbosity : int -> unit

      (* standard error: no backtrace is printed unless the backtrace_var
         env variable is defined. *)
      exception Error of string

    end ) : sig

    val main :
      ?on_error:(unit -> unit) ->
      ?print_config:(unit -> unit) ->
      sub list -> unit

  end = struct

    type cmd_node =
      {
        mutable node_cmd : sub option ;
        mutable node_map : cmd_node StringMap.t ref ;
        mutable node_commands : sub list ;
      }

    let echo () =
      Printf.eprintf "CMD: \x1B[1;33m %s \x1B[0m \n%!"
        ( String.concat " "
            ( List.filter (fun s -> s <> "--echo")
                ( Array.to_list Sys.argv )))

    let backtrace = match M.backtrace_var with
      | None -> false
      | Some v ->
          match Sys.getenv v with
          | _ -> true
          | exception _ -> false

    let backtrace = ref backtrace

    let increase_verbosity ()=
      let v = M.get_verbosity () in
      if v = 1 then begin
        Printexc.record_backtrace true;
        backtrace := true;
      end;
      M.set_verbosity ( v + 1 )

    let main
        ?(on_error = (fun () -> ()) )
        ?(print_config = (fun () -> ()) )
        subcommands =

      Printexc.record_backtrace !backtrace;

      let cmdmap subcommands =
        let cmdmap = ref StringMap.empty in
        let rec add_cmd map path cmd =
          match path with
            [] -> assert false
          | name :: path ->
              let node = match StringMap.find name !map with
                | exception Not_found ->
                    let node = {
                      node_cmd = None ;
                      node_map = ref StringMap.empty ;
                      node_commands = [] ;
                    } in
                    map := StringMap.add name node !map;
                    node
                | node -> node
              in
              node.node_commands <- cmd :: node.node_commands ;
              match path with
              | [] ->
                  assert ( node.node_cmd = None );
                  node.node_cmd <- Some cmd
              | _ -> add_cmd node.node_map path cmd
        in
        List.iter (fun ( path, cmd ) ->
            add_cmd cmdmap path cmd
          ) subcommands ;
        !cmdmap
      in

      let common_args =
        [
          [ "v"; "verbose" ],
          Unit increase_verbosity,
          info "Increase verbosity level" ;
          [ "q"; "quiet" ],
          Unit (fun () -> M.set_verbosity 0),
          info "Set verbosity level to 0";
        ]
      in
      let args = Array.to_list Sys.argv in

      let get_commands = function
        | Some (map, commands) -> (map, commands)
        | None ->
            let commands = List.map (fun cmd ->
                EzString.split cmd.sub_name ' ', cmd
              ) subcommands in
            let map = cmdmap commands in
            let commands = List.map snd commands in
            (map,commands)
      in

      let rec iter_initial_args path
          ( commands : (cmd_node StringMap.t * sub list ) option ) args =
        match args with
        | [] ->
            begin
              match path with
              | [] ->
                  Printf.eprintf "Use '%s --help' for help on commands\n%!"
                    M.command;
                  print_config ();
                  exit 0
              | _ ->
                  let _map, commands = get_commands commands in
                  path, args, commands
            end
        | "--echo" :: args ->
            echo () ;
            iter_initial_args path commands args
        | [ "--version" ] ->
            Printf.printf "%s\n%!" M.version;
            exit 0
        | [ "--about" ] ->
            Printf.printf "%s\n%!" M.about;
            exit 0
        | ( "-v" | "--verbose" ) :: args ->
            increase_verbosity ();
            iter_initial_args path commands args
        | ( "-q" | "--quiet" ) :: args ->
            M.set_verbosity 0;
            iter_initial_args path commands args
        | [ "rst" ] ->
            let _map, commands = get_commands commands in
            Printf.printf "%s%!" ( to_rst commands common_args );
            exit 0
        | name :: rem_args ->
            let map, commands = get_commands commands in
            match StringMap.find name map with
            | exception Not_found -> path, args, commands
            | node ->
                iter_initial_args
                  ( name :: path )
                  ( Some (! (node.node_map) , node.node_commands ))
                  rem_args
      in

      let path, args, ez_commands =
        iter_initial_args [] None (List.tl args ) in
      let args = match path with
        | [] -> args
        | path -> ( String.concat " " (List.rev path) ) :: args
      in
      let argv = Array.of_list ( Sys.argv.(0) :: args ) in
      try
        begin
          match args with
          | [] -> ()
          | _ ->
              main_with_subcommands
                ~name:M.command ~version:M.version
                ~doc:M.usage
                ~man:[] ~argv ez_commands
                ~common_args;
        end
      with
      | M.Error s when not !backtrace ->
          on_error () ;
          Printf.eprintf "Fatal error: %s\n%!" s;
          exit 2
      | exn ->
          on_error ();
          let bt = Printexc.get_backtrace () in
          let error = Printexc.to_string exn in
          Printf.eprintf "fatal exception %s\n%s\n%!" error bt;
          exit 2

  end

end
