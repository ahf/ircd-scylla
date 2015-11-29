(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

module Make (C : V1_LWT.CONSOLE) (S : V1_LWT.STACKV4) (KV : V1_LWT.KV_RO) :
    sig
        module Settings : module type of Settings.Make (C)
        module Log      : module type of Log.Make (C)

        type t

        val create : C.t -> Settings.t -> KV.t -> S.t -> t

        val log : t -> Log.Level.t -> string -> unit

        val kv : t -> KV.t
        val stack : t -> S.t
        val settings : t -> Settings.t
    end
