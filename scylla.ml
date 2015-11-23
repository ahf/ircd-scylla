(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

open Lwt

open V1
open V1_LWT

module Make (C : CONSOLE) (S : STACKV4) (KV : KV_RO) =
    struct
        module Logger = Log.Make (C)

        type t =
            {
                log    : Logger.t;
                config : Conf.t;
                kv     : KV.t;
                stack  : S.t;
            }

        let create console config kv stack =
            let log = Logger.create (Conf.log_level config) console in
            {
                log;
                config;
                kv;
                stack;
            }

        let log t level s =
            Logger.log t.log level s

        let kv t =
            t.kv

        let stack t =
            t.stack

        let config t =
            t.config
    end
