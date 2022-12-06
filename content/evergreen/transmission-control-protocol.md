+++
title = "Transmission Control Protocol (TCP)"
author = ["Chloe"]
date = 2022-12-06T12:00:00-05:00
lastmod = 2022-12-06T12:00:30-05:00
tags = ["concept"]
draft = false
weight = 1004
+++

TCP (Transmission Control Protocol): A protocol that provides a
reliable, ordered delivery of data over an IP network. TCP uses a
three-way handshake to establish a connection and uses sequence
numbers to ensure that data is delivered in the correct order.


## TCP socket stream {#tcp-socket-stream}

A TCP (Transmission Control Protocol) socket is a type of network
socket that is used to create a connection between a client and a
server. A socket is a software endpoint that establishes bidirectional
communication between a server and one or more clients. TCP sockets
are a specific type of socket that uses the TCP protocol for
transmitting data.

A socket stream is a flow of data that is transmitted between a client
and a server using a TCP socket. When a client establishes a
connection with a server using a TCP socket, a socket stream is
created that allows the two parties to send and receive data over the
connection. The socket stream provides a reliable, ordered, and
error-checked transmission of data between the client and server.

TCP socket streams are used in many different applications, including
web browsing, email, and file transfer. They are an essential part of
the underlying infrastructure that makes the internet possible.


## A TCP connection will be established after the TLS handshaking {#a-tcp-connection-will-be-established-after-the-tls-handshaking}

If the browser was written by Google, the browser might negotiate with
the server to upgrade from HTTP to SPDY protocol[^fn:1]. The later
version of Chrome favors HTTP/2 instead of SPDY protocol.&nbsp;[^fn:2]

The browser will send a GET request to fetch content, the GET request
contains information that can

-   identify the browser
-   State what type of response it will accept
-   the connection header asking whether TCP connection should remain
    open for further requests
-   send cookies[^fn:3] the browser has for the domain.

HTTP/1.1[^fn:4] defines the "close" connection option for the sender to
signal that the connection will be closed after completion of the
response. For example, Connection: close.

[^fn:1]: SPDY is a protocol designed to reduce web page load
    latency. SPDY does this by multiplexing multiple HTTP requests into a
    single connection, compressing request and response headers, and
    prioritizing resources.
[^fn:2]: HTTP/2 is an application-level protocol for sending and
    receiving data over the Internet. It is an updated version of
    HTTP/1.1, and is designed to reduce latency and improve web
    performance. It supports multiplexing, header compression, and server
    push technology, among other features.
[^fn:3]: Cookies are key-value pairs that track the state of a website
    between different page requests. Cookies store the name of the
    logged-in user, a secret number that was assigned to the user by the
    server, some user's setting etc. The cookies will be stored in a text
    file on the client, and sent to the server with every request.
[^fn:4]: HTTP/1.1 is a version of the Hypertext Transfer Protocol (HTTP)
    used for communication between web browsers and web servers. It is an
    application-level protocol for distributed, collaborative, hypermedia
    information systems. It was first defined in RFC 2068 in 1997, and has
    since been updated several times. It is the most widely used version
    of HTTP, and is the basis for other HTTP versions such as HTTP/2.