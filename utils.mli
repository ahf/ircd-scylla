(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

val intersperse : 'a list -> 'a -> 'a list
val split : string -> char -> string * string
val prefix : string -> char -> bool
val split_all : string -> char -> string list
