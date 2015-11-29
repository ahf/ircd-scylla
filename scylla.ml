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
        module Log      = Log.Make (C)
        module Settings = Settings.Make (C)

        type t = {
            log      : Log.t;
            settings : Settings.t;
            kv       : KV.t;
            stack    : S.t;
        }

        let create console settings kv stack =
            let log_level = Settings.log_level settings in
            let log = Log.create log_level console in
            {
                log;
                settings;
                kv;
                stack;
            }

        let log t level s =
            Log.log t.log level s

        let kv t =
            t.kv

        let stack t =
            t.stack

        let settings t =
            t.settings
    end
