+++
title = "Transport Layer Security (TLS)"
author = ["Chloe"]
date = 2022-12-06T12:00:00-05:00
lastmod = 2022-12-06T12:00:38-05:00
tags = ["concept"]
draft = false
weight = 1005
+++

## What is TLS {#what-is-tls}

Transport Layer Security (TLS) is a cryptographic protocol designed to
provide secure communication over a computer network. It is widely
used in web browsers and other applications. TLS versions refer to the
different versions of the TLS protocol, which have been released over
the years and provide varying levels of security. The latest version
is TLS 1.3, which was published in 2018.


## The Process of TLS handshaking {#the-process-of-tls-handshaking}

In the process of establishing an HTTPS connection, the client and
server both exchange data about the TLS version, cipher algorithms,
compression methods, certificates, and public keys in order to create
a secure, encrypted connection using a symmetric key. This key is then
used to encrypt the application (HTTP) data, allowing it to be
securely transmitted.&nbsp;[^fn:1]


## The importance of TLS {#the-importance-of-tls}

The TLS (Transport Layer Security) handshake is the process that
establishes a secure communication link between a client and a
server. The handshake involves several steps that enable the client
and server to authenticate each other and negotiate the encryption
keys that will be used to secure the communication.

The TLS handshake is necessary because it establishes a secure
connection between the client and server, protecting the data that is
transmitted between them. Without the handshake, the data transmitted
between the client and server would be vulnerable to interception and
tampering by third parties.

The TLS handshake also enables the client and server to negotiate the
encryption keys that will be used to secure the communication. This is
important because the encryption keys must be unique for each session
and must be known only to the client and server. The handshake ensures
that the keys are exchanged securely and that only the client and
server have access to them.

Overall, the TLS handshake is essential for establishing secure
communication between a client and a server on the internet. It
protects the data transmitted between the two parties and enables them
to negotiate the encryption keys that will be used to secure the
communication.

[^fn:1]: More detailed version can be seen [here](https://github.com/vasanthk/how-web-works#opening-of-a-socket--tls-handshake)