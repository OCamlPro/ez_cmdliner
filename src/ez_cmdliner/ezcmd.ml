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

module V2 = struct

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

    module RAWTYPES = struct
      include TYPES
    end
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

    let main_with_subcommands ~name ?version ?default ~doc ?(man=[]) ?(topics = []) ?(common_args=[]) subs
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
      match Term.eval_choice ~catch:false default_cmd cmds with
      | `Ok () -> ()
      | t -> Term.exit t

    let main ?version cmd =
      let cmd = create_sub ?version cmd ~common_args:[] in
      match Term.eval ~catch:false cmd with
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
  end
end

module V1 = struct

  module Ezcmd = struct
    include V2.EZCMD
    open TYPES

    let info ?docs ?docv ?env s = info ?docs ?docv ?env s

    let sub_of_cmd cmd =
      {
        sub_name = cmd.cmd_name ;
        sub_action = cmd.cmd_action ;
        sub_args = cmd.cmd_args ;
        sub_man = cmd.cmd_man ;
        sub_version = None ;
        sub_doc = cmd.cmd_doc ;
      }

    let main_with_subcommands ~name ?version ?default ~doc ?man ?topics cmds =
      let default = match default with
        | None -> None
        | Some cmd -> Some ( sub_of_cmd cmd ) in
      main_with_subcommands ~name ?version ?default ~doc ?man ?topics
        ( List.map sub_of_cmd cmds )

    let main ?version cmd = main ?version (sub_of_cmd cmd)
  end
end

include V1.Ezcmd
let main_with_subcommands2 = V2.EZCMD.main_with_subcommands
