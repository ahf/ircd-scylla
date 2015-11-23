(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

open Sexplib.Std

module Level =
    struct
        type t =
            | Debug
            | Info
            | Notice
            | Warning
            | Error with sexp

        let to_string log_level =
            match log_level with
            | Debug   -> "debug"
            | Info    -> "info"
            | Notice  -> "notice"
            | Warning -> "warning"
            | Error   -> "error"

        exception LogLevelError of string

        let from_string s =
            match (String.lowercase s) with
            | "debug"   -> Debug
            | "info"    -> Info
            | "notice"  -> Notice
            | "warning" -> Warning
            | "error"   -> Error
            | _         -> raise (LogLevelError s)

        let to_colour log_level =
            match log_level with
            | Debug   -> Colour.Gray
            | Info    -> Colour.Cyan
            | Notice  -> Colour.Purple
            | Warning -> Colour.Yellow
            | Error   -> Colour.Red

        let to_integer log_level =
            match log_level with
            | Debug   -> 0
            | Info    -> 1
            | Notice  -> 2
            | Warning -> 3
            | Error   -> 4
    end

module Make (C : V1_LWT.CONSOLE) =
    struct
        open Clock

        type t =
            {
                min_log_level : Level.t;
                console       : C.t;
            }

        let create min_log_level console =
            {
                min_log_level;
                console
            }

        let log logger level s =
            if (Level.to_integer level) >= (Level.to_integer logger.min_log_level) then
                let t = Clock.gmtime (Clock.time ()) in
                let timestamp = Printf.sprintf "%02d/%02d/%d %02d:%02d:%02d" t.tm_mday (t.tm_mon + 1) (t.tm_year + 1900) t.tm_hour t.tm_min t.tm_sec in
                let info = Printf.sprintf "%s [%s] " timestamp (Level.to_string level) in
                let message = Colour.format (Level.to_colour level) s in
                C.log logger.console (info ^ message)
            else
                ()
    end
