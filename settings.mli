(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

module Make (C : V1_LWT.CONSOLE) :
    sig
        module Log : module type of Log.Make (C)

        type t

        val create : string -> int -> Log.Level.t -> t

        val name : t -> string
        val port : t -> int
        val log_level : t -> Log.Level.t
    end
