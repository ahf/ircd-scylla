(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

type t =
    {
        nickname : string;
        username : string;
        hostname : string;
    }

let create nickname username hostname =
    {
        nickname;
        username;
        hostname;
    }

let nickname { nickname; _ } =
    nickname

let username { username; _ } =
    username

let hostname { hostname; _ } =
    hostname

let to_string { nickname; username; hostname } =
    nickname ^ "!" ^ username ^ "@" ^ hostname

exception ParseError of string

let parse s =
    try
        let at = String.index s '@' in
        let exclamation_mark = String.index s '!' in
        let length = String.length s in
        {
            nickname = String.sub s 0 exclamation_mark;
            username = String.sub s (exclamation_mark + 1) (at - exclamation_mark - 1);
            hostname = String.sub s (at + 1) (length - at - 1);
        }
    with Not_found ->
        raise (ParseError s)
