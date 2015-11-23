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
            t.output_push (Some (s ^ "\r\n"))

        let connected t =
            log t Log.Level.Info "Connected ..."

        let disconnected t =
            log t Log.Level.Info "Disconnected ..."

        let maybe_register_client t =
            let server_name = Conf.name (Scylla.config t.scylla) in
            let username = t.username in
            let nickname = t.nickname in
            match (username, nickname) with
            | ("*", _) -> t
            | (_, "*") -> t
            | _        ->
                    log t Log.Level.Info "Registered ...";
                    write t (Printf.sprintf ":%s 001 %s :Welcome to the IRC network, %s" server_name nickname nickname);
                    { t with registered = true }

        let handle_registered_message t m =
            let command = Message.command m in
            let server_name = Conf.name (Scylla.config t.scylla) in
            match command with
            | "PING" ->
                    (match (Message.arguments m) with
                     | d :: [] ->
                             write t (Printf.sprintf ":%s PONG :%s" server_name d);
                             t
                     | _ ->
                             t)
            | _ ->
                    write t (Printf.sprintf ":%s 421 %s %s :Unknown command" server_name t.nickname command);
                    t

        let handle_unregistered_message t m =
            let new_t = match (Message.command m) with
                        | "NICK" ->
                                (match (Message.arguments m) with
                                | nick :: [] ->
                                       { t with nickname = nick }
                                | _ ->
                                        t)
                        | "USER" ->
                                (match (Message.arguments m) with
                                | user :: _ :: _ :: real :: [] ->
                                        { t with username = "~" ^ user; realname = real }
                                | _ ->
                                        t)
                        | _ ->
                                t in
            maybe_register_client new_t

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

        let rec handle client =
            let tls = client.tls in
            let rec write () =
                    log client Log.Level.Error "write () ...";
                    let stream = client.output_stream in
                    let messages = Lwt_stream.get_available stream in
                    let s = String.concat "" messages in
                    let c = Cstruct.of_string s in
                    TLS.write tls c in

            log client Log.Level.Error "read () ...";
            lwt res = TLS.read tls in
            match res with
            | `Ok buffer ->
                    let message = client.cont ^ (Cstruct.to_string buffer) in
                    let new_client = handle_data client message in
                    write () >> handle new_client
            | `Error s ->
                    disconnected client;
                    return (`Error s)
            | `Eof ->
                    disconnected client;
                    return `Eof
    end
