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

  let main_with_subcommands
      ~name ?version ?default ~doc ?man ?topics ?argv cmds =
    let default = match default with
      | None -> None
      | Some cmd -> Some ( sub_of_cmd cmd ) in
    main_with_subcommands ~name ?version ?default ~doc ?man ?topics ?argv
      ( List.map sub_of_cmd cmds )

  let main ?version ?argv cmd = main ?version ?argv (sub_of_cmd cmd)
end
