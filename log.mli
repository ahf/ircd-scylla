(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

open Sexplib.Std

module Make (C : V1_LWT.CONSOLE) :
    sig
        module Level :
            sig
                type t =
                    | Debug
                    | Info
                    | Notice
                    | Warning
                    | Error

                exception LogLevelError of string

                val to_string : t -> string
                val from_string : string -> t

                val to_colour : t -> Colour.t

                val to_integer : t -> int
            end

        type t

        val create : Level.t -> C.t -> t
        val log : t -> Level.t -> string -> unit
    end
