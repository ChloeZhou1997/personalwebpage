+++
title = "Hypertext Transfer Protocol (HTTP)"
author = ["Chloe"]
date = 2022-12-06T12:00:00-05:00
lastmod = 2022-12-06T12:00:24-05:00
tags = ["concept"]
draft = false
weight = 1003
+++

## What is HTTP {#what-is-http}

Hyper Text Transfer Protocol (HTTP) is a protocol for transferring
files on the World Wide Web. It is the foundation of data
communication for the World Wide Web. It is a set of rules that
defines how data is transmitted and formatted between web clients and
servers. HTTP is a stateless protocol, meaning that each request and
response are independent of each other and do not maintain any
information about previous requests or responses.


## How to make sure the machine complies with the protocol? {#how-to-make-sure-the-machine-complies-with-the-protocol}

To make sure that a machine complies with the HTTP protocol, it must
be programmed to follow the rules and conventions defined by the
protocol. This typically involves implementing the appropriate
networking code to send and receive HTTP requests and responses, as
well as formatting and parsing the data according to the rules of the
protocol. In some cases, software libraries or frameworks may be used
to help implement HTTP in a machine.


## HTTP Strict Transport Security (HSTS) {#http-strict-transport-security--hsts}


### What is HSTS {#what-is-hsts}

HTTP Strict Transport Security (HSTS) is a security feature that tells
a web browser to only interact with a website using a secure HTTPS
connection, rather than using an insecure HTTP connection. This helps
protect against man-in-the-middle attacks and other forms of malicious
activity.


### Where is the HSTS policy stored? {#where-is-the-hsts-policy-stored}

The HSTS policy for a website is typically stored in the website's
server as a part of the HTTP response headers. When a web browser
makes an HTTP request to a website that uses HSTS, the server sends
back a response header that includes the HSTS policy, instructing the
browser to only use HTTPS for future requests to the website. The web
browser then stores this information locally and uses it to enforce
the HSTS policy. This ensures that the web browser only sends requests
to the website using a secure HTTPS connection.

However, the first HTTP request to the website by a user will receive
a response requesting that the user only send HTTPS requests. However,
this single HTTP request could potentially leave the user vulnerable
to a downgrade attack[^fn:1], which is why the HSTS list is included in
modern web browsers.


### What is preload HSTS list and when to check it? {#what-is-preload-hsts-list-and-when-to-check-it}

The HSTS preload list is a list of websites that have declared a
commitment to always use HTTPS for secure communication. The list is
maintained by the Chromium project and is used by web browsers to
enforce the HSTS policy for websites on the list.

To be included on the HSTS preload list, a website must meet certain
requirements, such as serving a valid HTTPS certificate and having a
valid HSTS policy. Once a website is added to the preload list, web
browsers that support HSTS will automatically enforce the HTTPS-only
policy for the website, even if the user has never visited the website
before. This helps ensure that users can only access the website using
a secure connection.

The preload list is updated regularly, and website owners can submit
their websites for inclusion on the list.

When sending a request to the server for content, the browser first
checks its preloaded HSTS list. If the website is on the list, the
browser will send its request via HTTPS instead of HTTP.

[^fn:1]: A downgrade attack is a type of attack where an attacker tricks
    a victim into using an older, weaker version of a protocol. This can
    be done by impersonating a server that only supports the older
    protocol, or by interfering with the victim's connection and removing
    any references to the newer protocol. Downgrade attacks can be used to
    exploit vulnerabilities in the older protocol, or to bypass security
    features that are only present in the newer protocol.