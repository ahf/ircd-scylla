(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

type t

val create : string -> string -> string -> t

val nickname : t -> string
val username : t -> string
val hostname : t -> string

val to_string : t -> string

val parse : string -> t

exception ParseError of string
