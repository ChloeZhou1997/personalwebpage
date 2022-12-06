+++
title = "DNS (Domain Name Server)"
author = ["Chloe"]
date = 2022-12-06T12:00:00-05:00
lastmod = 2022-12-06T12:00:14-05:00
tags = ["concept"]
draft = false
weight = 1002
+++

## What is a DNS server {#what-is-a-dns-server}

A domain name server (DNS) is a server that translates human-readable
domain names into the numerical IP addresses that computers use to
communicate with each other on the internet. When you enter a domain
name into your web browser's address bar, your computer sends a
request to the DNS server to look up the corresponding IP address for
the website. The DNS server then responds with the IP address, and
your computer uses that address to send a request for the website's
content to the appropriate server. This process allows you to access
websites using easy-to-remember domain names instead of having to
remember the numerical IP addresses of the servers hosting those
websites.


## Where is the DNS server? {#where-is-the-dns-server}

Because DNS servers are distributed across the internet, there is no
single location where they are all located. DNS servers are located in
many different places, including data centers, internet exchange
points, and other locations that are connected to the internet.


## The process of figuring out the corresponding IP address {#the-process-of-figuring-out-the-corresponding-ip-address}

When a browser needs to access a website, it first checks its own
cache to see if the DNS record is stored there. If not, it will then
make a system call to the OS cache&nbsp;[^fn:1], followed by the router
cache and ISP DNS cache[^fn:2], before initiating a recursive search
beginning with the root nameserver. The DNS server will often have the
.com nameservers in cache, which can speed up the process because
there's no need to hit the root.

{{< figure src="/images/DNS_(Domain_Name_Server)/org_20221205-010837_screenshot.png" width="400px" >}}


## Why is it worrisome if the entire domain only points to a single IP address? {#why-is-it-worrisome-if-the-entire-domain-only-points-to-a-single-ip-address}

Having an entire domain map to a single IP address can be worrying
because if the IP address is lost or compromised, the entire domain
could go down. This could mean that any websites, services, and
applications associated with the domain would become inaccessible. It
also makes it easier for malicious actors to target a domain, as they
only need to target the single IP address instead of multiple IP
addresses.


## The solutions regarding the map between the domain address and IP address {#the-solutions-regarding-the-map-between-the-domain-address-and-ip-address}

Round-robin DNS is a way of assigning multiple IP addresses to a
single domain name, while **load-balancers** are pieces of hardware used
to forward requests to other servers. Geographic DNS is used to map a
domain name to different IP addresses based on a user's location, and
anycast[^fn:3] is a routing technique where one IP address can be directed to
multiple physical servers, although it is not often used with TCP.

[^fn:1]: the browser makes system call and check whether OS cached it
[^fn:2]: ISP DNS cache: A DNS cache maintained by an Internet Service
    Provider (ISP) for faster DNS lookups. It stores the DNS records of
    recently visited websites so that the DNS lookup can be done from the
    cache instead of from the root nameserver. DNS caches help reduce the
    latency of DNS lookups, as they can be retrieved quickly from the
    cache without performing a recursive search.
[^fn:3]: Most of the DNS servers themselves use anycast to achieve high
    availability and low latency of the DNS lookups. Users of an anycast
    service (DNS is an excellent example) will always connect to the
    'closest' (from a routing protocol perspective) DNS server. This
    reduces latency, as well as provides a level of load-balancing
    (assuming that your consumers are evenly distributed around your
    network).