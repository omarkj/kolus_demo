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

Remember to use different ports.

Getting sockets via HTTP interface
----------------------------------

There are three ways of getting the sockets using two different endpoints:

### `/one`

Simply takes a socket and does nothing with it (but the status change is 
reflected on the `/backends` page)

### `/two`

Connects to the mock backend. These mock backends always reply with the port 
they are running on. It always connects to the first backend in the list from
`kolus:status`.

### `/three`

Also connects to the mock backend, but idle connections have priority, if no
idle connections are available it connects to the backend with the most unused
connection.

Both endpoints have the optional argument `time`. That argument specifies how
long the handler should wait before letting go of the socket.

`Hackney` modules
-----------------

This demo includes two `hackney` transport modules so it can be used with the
`kolus` library.

### `kolus_demo_tcp_transport`

This transport module always connects to the first backend in the kolus status list.

### `kolus_demo_idle_first_tcp_transport`

This transport module connects to idle connections if any exists, if not it connects
to the backend with the most unused connections.
