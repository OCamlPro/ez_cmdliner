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

(* Easy interface over Cmdliner. Very similar to the Stdlib Arg
   module.

   Three main functions: * [Ezcmd.main cmd]: simple command *
   [Ezcmd.main_with_subcommands cmds]: with sub-commands *
   [Ezcmd.Arg.parse]: similar to Arg.parse

   Usually, you will start your code with `open Ezcmd.TYPES` and then
   use functions and constructors from either `Ezcmd` or `Arg`. Also
   `Ezcmd.translate` can be used to translate from Stdlib Arg.

   This module is versioned, so we STRONGLY advise to use version
   submodules V1, V2, etc. :

   open Ezcmd.V1  or open Ezcmd.V2
   open Ezcmd.TYPES
   ...

*)

module V2 : sig

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

      type env
      type info

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

      type sub
    end

    open TYPES
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

    val sub : string ->
      doc:string ->
      ?args: arg_list ->
      ?man: block list ->
      ?version:string -> (* exists since *)
      (unit -> unit) ->
      sub

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

    module RAWTYPES : sig

      open TYPES

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

      type sub = {
        sub_name : string ;
        sub_action : unit -> unit;
        sub_args : arg_list;
        sub_man : block list;
        sub_version : string option ;
        sub_doc : string;
      }

    end

    val raw_env : TYPES.env -> RAWTYPES.env
    val raw_info : TYPES.info -> RAWTYPES.info
    val raw_sub : TYPES.sub -> RAWTYPES.sub

    val to_rst : ?name:string -> TYPES.sub list -> arg_list -> string
  end
end


module V1 : sig
  module Ezcmd : sig
    module TYPES : sig
      type block =
        [ `S of string
        | `P of string
        | `Pre of string
        | `I of string * string
        | `Noblank
        | `Blocks of block list ]

      type env
      type info
      type sub

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

      (* only for backward compatibility *)
      type command = {
        cmd_name : string;
        cmd_action : unit -> unit;
        cmd_args : arg_list;
        cmd_man : block list;
        cmd_doc : string;
      }

    end

    open TYPES
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

    val info : ?docs:string -> ?docv:string -> ?env:env -> string -> (* doc *)
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

    val main_with_subcommands :
      name:string ->
      (* name of main command *)
      ?version:string ->
      ?default:command ->
      (* if absent, prints help *)
      doc:string ->
      ?man:block list ->
      ?topics:(string * Cmdliner.Manpage.block list) list ->
      ?argv: string array ->
      command list ->
      unit

    val main :
      ?version:string ->
      ?argv: string array ->
      command -> unit

  end
end

include (module type of V1.Ezcmd)

val main_with_subcommands2 :
  name:string ->
  ?version:string ->
  ?default:TYPES.sub ->
  doc:string ->
  ?man:MANPAGE.block list ->
  ?topics:(string * MANPAGE.block list) list ->
  ?common_args: TYPES.arg_list ->
  ?argv: string array ->
  TYPES.sub list -> unit
