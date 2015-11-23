open Mirage

let env_dir = "env"

let disk =
    match get_mode () with
    | `Unix   -> direct_kv_ro env_dir
    | `MacOSX -> direct_kv_ro env_dir
    | `Xen    -> crunch env_dir

let net =
    try
        match Sys.getenv "NET" with
        | "direct" -> `Direct
        | _        -> `Socket
    with Not_found ->
        match get_mode () with
        | `Unix   -> `Socket
        | `MacOSX -> `Socket
        | `Xen    -> `Direct

let dhcp =
    try
        match Sys.getenv "ADDR" with
        | "dhcp"   -> `Dhcp
        | "static" -> `Static
    with Not_found -> `Static

let stack console =
    match net, dhcp with
    | `Direct, `Dhcp   -> direct_stackv4_with_dhcp console tap0
    | `Direct, `Static -> direct_stackv4_with_default_ipv4 console tap0
    | `Socket, _       -> socket_stackv4 console [Ipaddr.V4.any]

let server =
    foreign "Unikernel.Main" @@ console @-> stackv4 @-> kv_ro @-> job

let () =
    let platform =
        match get_mode () with
        | `Unix   -> "unix"
        | `MacOSX -> "unix"
        | `Xen    -> "xen" in
    add_to_opam_packages [ "mirage-clock-" ^ platform; "tls" ] ;
    add_to_ocamlfind_libraries [ "mirage-clock-" ^ platform; "tls"; "tls.mirage"; "sexplib"; "sexplib.syntax" ] ;
    register "scylla" [ server $ default_console $ stack default_console $ disk ]
