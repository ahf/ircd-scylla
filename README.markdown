# Scylla

Scylla is a Unikernel IRC daemon for MirageOS.

## Building and Running

    $ mirage configure --unix
    $ make
    $ ./mir-scylla

Now connect your IRC client to 127.0.0.1:6697 using TLS.
