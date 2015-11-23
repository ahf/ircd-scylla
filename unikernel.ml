(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

open Lwt

open V1
open V1_LWT

type ('a, 'e, 'c) m = ([< `Ok of 'a | `Error of 'e | `Eof ] as 'c) Lwt.t

let (>>==) (a : ('a, 'e, _) m) (f : 'a -> ('b, 'e, _) m) : ('b, 'e, _) m =
  a >>= function
    | `Ok x                -> f x
    | `Error _ | `Eof as e -> return e

module Main (C : CONSOLE) (S : STACKV4) (KV : KV_RO) =
    struct
        module Scylla   = Scylla.Make (C) (S) (KV)
        module Listener = Listener.Make (C) (S) (KV)

        let start c stack kv =
            let config = Conf.create "scylla.0x90.dk" 6697 Log.Level.Debug in
            let scylla = Scylla.create c config kv stack in
            Listener.listen scylla
    end
