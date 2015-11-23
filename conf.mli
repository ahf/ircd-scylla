(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

type t

val create : string -> int -> Log.Level.t -> t
val name : t -> string
val port : t -> int
val log_level : t -> Log.Level.t
