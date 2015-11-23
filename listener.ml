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
        module X509   = Tls_mirage.X509 (KV) (Clock)
        module Scylla = Scylla.Make (C) (S) (KV)
        module Client = Client.Make (C) (S) (KV)

        type ('a, 'e, 'c) m = ([< `Ok of 'a | `Error of 'e | `Eof ] as 'c) Lwt.t

        let (>>==) (a : ('a, 'e, _) m) (f : 'a -> ('b, 'e, _) m) : ('b, 'e, _) m =
          a >>= function
            | `Ok x                -> f x
            | `Error _ | `Eof as e -> return e

        let accept scylla conf flow =
            TLS.server_of_flow conf flow >>==
                (fun tls ->
                    let ip, port = S.TCPV4.get_dest flow in
                    let client = Client.create scylla ip port tls in
                    Client.connected client;
                    Client.handle client)
                >>= function
                    | `Error _ | `Eof ->
                            fail (Failure "tls init")
                    | `Ok _ ->
                            assert false

        let listen scylla =
            let stack = Scylla.stack scylla in
            let kv = Scylla.kv scylla in
            let port = Conf.port (Scylla.config scylla) in
            lwt cert = X509.certificate kv `Default in
            let conf = Tls.Config.server ~certificates:(`Single cert) () in
            S.listen_tcpv4 stack port (accept scylla conf) ;
            S.listen stack
    end
