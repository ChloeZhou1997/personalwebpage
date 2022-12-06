+++
title = "What happened behind the scene when you type an url and request the browser for information"
author = ["Chloe"]
date = 2022-12-05
lastmod = 2022-12-05T22:26:05-05:00
draft = true
weight = 1006
+++

-   **parse** the URL by first, check the protocol (usually [HTTP]({{< relref "hyptertext-transfer-protocol" >}})), and
    retrieve the main (index) page (from "/")
-   the browser sends a request to a [Domain Name Server]({{< relref "domain-name-server" >}})
-   translate the domain name into an IP address (the domain name server
    response with the ip address of the website)
-   once the browser receives the IP address, opening of a socket ([TCP
    socket]({{< relref "transmission-control-protocol" >}})) + [TLS]({{< relref "transport-layer-security" >}}) handshake (to establish the secure connection)
-   browser send a GET request to the IP address for the website's
    content. ([more info]({{< relref "transmission-control-protocol#a-tcp-connection-will-be-established-after-the-tls-handshaking" >}}))
-   sending the requested content back to the browser
-   browser display content
