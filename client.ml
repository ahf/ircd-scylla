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
        module TLS      = Tls_mirage.Make (S.TCPV4)
        module Scylla   = Scylla.Make (C) (S) (KV)
        module Settings = Settings.Make (C)
        module Log      = Log.Make (C)

        type write_command = Stop | Message of string

        type t =
            {
                scylla     : Scylla.t;

                mutable nickname   : string;
                mutable username   : string;
                mutable realname   : string;

                hostname   : string;

                mutable tls : TLS.flow;

                ip         : S.TCPV4.ipaddr;
                port       : int;

                cont       : string;

                registered : bool;

                mailbox    : write_command Lwt_mvar.t;
            }

        let log t log_level s =
            let message = Printf.sprintf "Client %s!%s@%s:%d: %s" t.nickname t.username t.hostname t.port s in
            Scylla.log t.scylla log_level message

        let connected t =
            log t Log.Level.Info "Connected ..."

        let disconnected t =
            log t Log.Level.Info "Disconnected ..."

        let create scylla ip port tls =
            let mailbox = Lwt_mvar.create_empty () in
            {
                scylla = scylla;

                nickname = "*";
                username = "*";
                realname = "";

                hostname = Ipaddr.V4.to_string ip;

                tls = tls;

                ip = ip;
                port = port;

                cont = "";

                registered = false;

                mailbox = mailbox;
            }

        let write_command t cmd =
            let _ = Lwt_mvar.put t.mailbox cmd in
            ()

        let write t m =
            write_command t (Message m)

        let close_writer t =
            write_command t Stop

        let maybe_register_client t =
            let settings = Scylla.settings t.scylla in
            let server_name = Settings.name settings in
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
            let settings = Scylla.settings t.scylla in
            let server_name = Settings.name settings in
            let command = Message.command m in
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
                                        t.nickname <- nick;
                                        t
                                | _ ->
                                        t)
                        | "USER" ->
                                (match (Message.arguments m) with
                                | user :: _ :: _ :: real :: [] ->
                                        t.username <- "~" ^ user;
                                        t.realname <- real;
                                        t
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

        let rec handle_write client =
            lwt command = Lwt_mvar.take client.mailbox in
            match command with
            | Message message ->
                log client Log.Level.Debug (">> " ^ message);
                (lwt res = TLS.write client.tls (Cstruct.of_string (message ^ "\r\n")) in
                match res with
                | `Error e -> log client Log.Level.Error ("Write error: " ^ (TLS.error_message e));
                              return_unit
                | `Eof     -> return_unit
                | `Ok _    -> handle_write client)
            | Stop -> return_unit

        let rec handle_read client =
            lwt res = TLS.read client.tls in
            match res with
            | `Error e   -> log client Log.Level.Error ("Read error: " ^ (TLS.error_message e));
                            close_writer client;
                            return_unit
            | `Eof       -> close_writer client;
                            return_unit
            | `Ok buffer ->
                    let message = client.cont ^ (Cstruct.to_string buffer) in
                    let new_client = handle_data client message in
                    handle_read new_client

        let handle client =
            connected client;
            join [
                    handle_read client;
                    handle_write client
                 ] >|= fun () -> disconnected client
    end
