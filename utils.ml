(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

let rec intersperse list element =
    match list with
    | [] | [_]       -> list
    | a :: b :: tail -> a :: element :: intersperse (b :: tail) element

let split s c =
    let p = String.index s c in
    let l = String.length s in
    let first = String.sub s 0 p in
    let second = String.sub s (p + 1) (l - p - 1) in
    (first, second)

let prefix s c =
    match (String.length s) with
    | 0 -> false
    | _ -> (String.get s 0) == c
