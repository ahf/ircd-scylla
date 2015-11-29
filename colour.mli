(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

type t =
    | Red
    | Green
    | Yellow
    | Cyan
    | Purple
    | Gray

val format : t -> string -> string
