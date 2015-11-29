(*
 * Copyright (c) 2015 Alexander Færøy. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 *)

module Make (C : V1_LWT.CONSOLE) (S : V1_LWT.STACKV4) (KV : V1_LWT.KV_RO) :
    sig
        module Scylla : module type of Scylla.Make (C) (S) (KV)
        module TLS    : module type of Tls_mirage.Make (S.TCPV4)

        type t

        val create : Scylla.t -> S.TCPV4.ipaddr -> int -> TLS.flow -> t
        val handle : t -> unit Lwt.t
    end
