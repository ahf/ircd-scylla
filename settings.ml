(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

module Make (C : V1_LWT.CONSOLE) =
    struct
        module Log = Log.Make (C)

        type t = {
            name      : string;
            port      : int;
            log_level : Log.Level.t;
        }

        let create name port log_level = {
            name;
            port;
            log_level;
        }

        let name { name; _ } =
            name

        let port { port; _ } =
            port

        let log_level { log_level; _ } =
            log_level
    end
