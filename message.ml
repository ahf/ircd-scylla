(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

type t =
    {
        prefix    : string option;
        command   : string;
        arguments : string list;
    }

let prefix { prefix; _ } =
    prefix

let command { command; _ } =
    command

let arguments { arguments; _ } =
    arguments

exception ParseError of string

let rec parse_one_argument s l =
    match (Utils.prefix s ':') with
    | true ->
            (String.sub s 1 (String.length s - 1)) :: l
    | false ->
            match (String.contains s ' ') with
            | true ->
                    let argument, rest = Utils.split s ' ' in
                    parse_one_argument rest (argument :: l)
            | false ->
                    s :: l

let parse_arguments s =
    List.rev (parse_one_argument s [])

let parse_command prefix s =
    match (String.contains s ' ') with
    | true ->
            let command, rest = Utils.split s ' ' in
            {
                prefix    = prefix;
                command   = String.uppercase command;
                arguments = parse_arguments rest;
            }
    | false ->
            {
                prefix    = prefix;
                command   = String.uppercase s;
                arguments = [];
            }

let parse_prefix s =
    match (String.contains s ' ') with
    | true ->
            let prefix, rest = Utils.split s ' ' in
            parse_command (Some prefix) rest
    | false ->
            raise (ParseError s)

let parse s =
    try
        match (Utils.prefix s ':') with
        | true ->  parse_prefix (String.sub s 1 ((String.length s) - 1))
        | false -> parse_command None s
    with _ ->
        raise (ParseError s)

let to_string { prefix; command; arguments; } =
    let p =
        match prefix with
        | Some v -> (String.concat "" ["'"; v; "'"])
        | None   -> "N/A" in
    Printf.sprintf "Prefix: %s, Command: '%s', Arguments: '%s'" p command (String.concat "', '" arguments)
