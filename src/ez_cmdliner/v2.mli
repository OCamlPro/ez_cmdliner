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

(** Easy interface over Cmdliner. Very similar to the Stdlib Arg
   module.

   Three main functions:
   * [Ezcmd.main cmd]: simple command
   * [Ezcmd.main_with_subcommands cmds]: with sub-commands
   * [Ezcmd.Arg.parse]: similar to Arg.parse

   Usually, you will start your code with the line:

   {v open Ezcmd.V2 v}

   Then, use functions and constructors from either `EZCMD` module.

   If you use subcommands, use the [MAKE] functor.
*)

module EZCMD : sig

  module TYPES : sig
    (* Use Cmdliner doclang markup:
       $(b,XXX) bold
       $(i,XXX) italic
       $(docv)
    *)

    type block =
      [ `S of string (* section *)
      | `P of string (* paragraph *)
      | `Pre of string (* code *)
      | `I of string * string (* item (label, description) *)
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

    module Arg : sig
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

  open TYPES

  type spec = Arg.spec =
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

  module MANPAGE = Cmdliner.Manpage

  (* Partial Compatibility with Stdlib Arg module *)
  val parse :
    ?name:string ->
    ?version:string ->
    ?man:block list ->
    (string * Arg.spec * string) list ->
    (string -> unit) ->
    string ->
    unit

  val translate :
    ?docs:string ->
    (string * Arg.spec * string) list ->
    (string list * Arg.spec * info) list

  val translate_anon : (string -> unit) -> (string list * Arg.spec * info) list

  val env : ?docs:string -> ?doc:string -> string -> env
  (** [env ~docs ~doc var] describes an environment variable
        [var]. [doc] is the man page information of the environment
        variable, defaults to ["undocumented"]. [docs] is the title of
        the man page section in which the environment variable will be
        listed, it defaults to {!Manpage.s_environment}.

        In [doc] the {{!doclang}documentation markup language} can be
        used with following variables:
        {ul
        {- [$(env)], the value of [var].}
        {- The variables mentioned in {!info}}} *)

  val info : ?docs:string -> ?docv:string -> ?env:env -> ?version: string ->
    string -> (* doc *)
    info
  (** [info docs docv env doc] defines information for
        an argument.
        {ul
        {- [env] defines the name of an environment variable which is
           looked up for defining the argument if it is absent from the
           command line. See {{!envlookup}environment variables} for
           details.}
        {- [doc] is the man page information of the argument.
           The {{!doclang}documentation language} can be used and
           the following variables are recognized:
           {ul
           {- ["$(docv)"] the value of [docv] (see below).}
           {- ["$(opt)"], one of the options of [names], preference
              is given to a long one.}
           {- ["$(env)"], the environment var specified by [env] (if any).}}
           {{!doc_helpers}These functions} can help with formatting argument
           values.}
        {- [docv] is for positional and non-flag optional arguments.
           It is a variable name used in the man page to stand for their value.}
        {- [docs] is the title of the man page section in which the argument
           will be listed. For optional arguments this defaults
           to {!Manpage.s_options}. For positional arguments this defaults
           to {!Manpage.s_arguments}. However a positional argument is only
           listed if it has both a [doc] and [docv] specified.}} *)

  val sub :
    string -> (* subcommand *)
    doc:string ->
    ?args: arg_list ->
    ?man: block list ->
    ?version:string -> (* exists since *)
    (unit -> unit) ->
    sub
  (** [sub name action] associates the action [action] with the
     subcommand [name]. This module supports only one level of
     sub-commands, unless you are using the [MAKE] functor. With the
     [MAKE] functor, subcommand names can contain spaces. *)

  val main_with_subcommands :
    name:string ->
    (* name of main command *)
    ?version:string ->
    ?default:sub ->
    (* if absent, prints help *)
    doc:string ->
    ?man:block list ->
    ?topics:(string * Cmdliner.Manpage.block list) list ->
    ?common_args: arg_list ->
    ?argv: string array ->
    sub list ->
    unit

  val main :
    ?version:string ->
    ?argv: string array ->
    sub -> unit

  module RAWTYPES = TYPES

  val raw_env : TYPES.env -> RAWTYPES.env
  val raw_info : TYPES.info -> RAWTYPES.info
  val raw_sub : TYPES.sub -> RAWTYPES.sub

  val to_rst : ?name:string -> TYPES.sub list -> arg_list -> string



  module MAKE( M : sig

      (* command name *)
      val command : string

      (* current version *)
      val version : string

      (* printed when user calls with --about *)
      val about : string

      (* printed by cmdliner for doc *)
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

  (**
    This is the main function. It will dispatch on the subcommand called
    by the user.

    It takes a list of subcommands, defined for example with:

{v
open Ezcmd.V2

let cmd =
  let files = ref [] in
  EZCMD.sub
    "parse this file"
    (fun () ->
     ... (* action to perform after parsing options *)
    )
    ~args:
      [
        [ "a"; "after" ],
        EZCMD.String (fun s -> ... ),
        EZCMD.info ~docv:"STRING" "I use this string";

        [],
        EZCMD.Anons (fun args -> ... ),
        EZCMD.info ~docv:"FILE" "I use these arguments" ;
      ]
    ~doc: "..."
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "..."
      ];
    ]

let () = main [ cmd ]
v}

    default options:
     -v | --verbose : increase verbosity and backtraces
     -q | --quiet : set verbosity to 0
     --version : print M.version
     --about : print M.about
     --echo : print command with current arguments
     rst : output a .rst file with all subcommands
  *)

    val main :
      ?on_error:(unit -> unit) ->
      ?print_config:(unit -> unit) ->
      sub list -> unit

  end


end
