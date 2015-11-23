(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

type t

val prefix : t -> string option
val command : t -> string
val arguments : t -> string list

exception ParseError of string

val parse : string -> t

val to_string : t -> string
