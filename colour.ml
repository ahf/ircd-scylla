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

let format colour s =
    match colour with
    | Red    -> "\027[31m" ^ s ^ "\027[m"
    | Green  -> "\027[32m" ^ s ^ "\027[m"
    | Yellow -> "\027[33m" ^ s ^ "\027[m"
    | Cyan   -> "\027[36m" ^ s ^ "\027[m"
    | Purple -> "\027[35m" ^ s ^ "\027[m"
    | Gray   -> "\027[37m" ^ s ^ "\027[m"
