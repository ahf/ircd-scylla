(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

open Lwt

open V1
open V1_LWT

module Make (C : CONSOLE) (S : STACKV4) (KV : KV_RO) =
    struct
        module TLS    = Tls_mirage.Make (S.TCPV4)
        module Scylla = Scylla.Make (C) (S) (KV)

        type t =
            {
                scylla     : Scylla.t;

                nickname   : string;
                username   : string;
                hostname   : string;

                realname   : string;

                tls        : TLS.flow;

                ip         : S.TCPV4.ipaddr;
                port       : int;

                cont       : string;

                registered : bool;

                output_push   : (string option -> unit);
                output_stream : string Lwt_stream.t;
            }

        let create scylla ip port tls =
            let output_stream, output_push = Lwt_stream.create () in
            {
                scylla = scylla;

                nickname = "*";
                username = "*";
                hostname = Ipaddr.V4.to_string ip;

                realname = "";

                tls = tls;

                ip = ip;
                port = port;

                cont = "";

                registered = false;

                output_push = output_push;
                output_stream = output_stream;
            }

        let log t log_level s =
            let message = Printf.sprintf "Client %s!%s@%s:%d: %s" t.nickname t.username t.hostname t.port s in
            Scylla.log t.scylla log_level message

        let write t s =
            log t Log.Level.Debug (">> " ^ s);
            t.output_push (Some s)

        let connected t =
            log t Log.Level.Info "Connected ...";
            write t ":127.0.0.1 NOTICE * :*** Processing your connection a...";
            write t ":127.0.0.1 NOTICE * :*** Processing your connection b...";
            write t ":127.0.0.1 NOTICE * :*** Processing your connection c..."

        let disconnected t =
            log t Log.Level.Info "Disconnected ..."

        let handle_registered_message t m =
            t

        let handle_unregistered_message t m =
            t

        let handle_message t message =
            match t.registered with
            | true ->
                    handle_registered_message t message
            | false ->
                    handle_unregistered_message t message

        let handle_line t line =
            log t Log.Level.Debug ("<< " ^ line);
            try
                let message = Message.parse line in
                handle_message t message
            with (Message.ParseError e) ->
                log t Log.Level.Error e;
                t

        let rec handle_data t data =
            match (String.contains data '\n') with
            | true ->
                    let line, rest = Utils.split data '\n' in
                    let new_t = handle_line t (String.trim line) in
                    handle_data new_t rest
            | false ->
                    { t with cont = data }

        let rec handle t =
            let tls = t.tls in
            let rec write () =
                let stream = t.output_stream in
                Lwt_stream.get stream >>= fun m ->
                    match m with
                    | None -> handle t
                    | Some message ->
                        let s = message ^ "\r\n" in
                        let c = Cstruct.of_string s in
                        return (TLS.write tls c) >> write () in

            let read () =
                lwt res = TLS.read tls in
                match res with
                | `Ok buffer ->
                        let message = t.cont ^ (Cstruct.to_string buffer) in
                        let new_t = handle_data t message in
                        handle new_t
                | `Error s ->
                        log t Log.Level.Error "Error ...";
                        return (`Error s)
                | `Eof ->
                        disconnected t;
                        return `Eof in
            pick [read (); write ()]
    end
