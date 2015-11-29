(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

open Lwt

open V1
open V1_LWT

module Main (C : CONSOLE) (S : STACKV4) (KV : KV_RO) =
    struct
        module Scylla   = Scylla.Make (C) (S) (KV)
        module Listener = Listener.Make (C) (S) (KV)
        module Settings = Settings.Make (C)
        module Log      = Log.Make (C)

        let start console stack kv =
            let settings = Settings.create "scylla.0x90.dk" 6697 Log.Level.Debug in
            let scylla = Scylla.create console settings kv stack in
            Scylla.log scylla Log.Level.Info "Starting Scylla";
            Listener.listen scylla
    end
