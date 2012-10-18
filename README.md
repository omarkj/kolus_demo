kolus_demo
==========

A simple example of how `kolus` can be used.

Setup
-----

```
$ make
```

Run
---

```
$ make start
```

This will drop you into an erlang shell, it will also start a
mock inbound server on port 10001. A backend status page is at
the path `/backends`. It's not pretty.

Start backends
--------------

``` erlang
1> kolus_demo:start_backend(Port).
```

Getting sockets via HTTP interface
----------------------------------

There are two ways of getting the sockets using two different endpoints,
either `/one` that simply takes a socket and does nothing with it (but the
status change is reflected on the `/backends` page) or `/two` which connects
to the mock backend. These mock backends always reply with the port they are
running on.

Both endpoints have the optional argument `time`. That argument specifies how
long the handler should wait before letting go of the socket.

`Hackney` module
--------------

This demoe includes a `hackney` TCP module so `kolus` can be used with `kolus`,
the `/two` endpoint uses that module to get sockets and return them after use.

