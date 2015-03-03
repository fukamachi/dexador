# Dexador

Dexador is yet another HTTP client for Common Lisp with neat APIs and connection-pooling.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

```common-lisp
(dex:get "http://lisp.org/")

(dex:post "https://example.com/login"
          :content '(("name" . "fukamachi") ("password" . "1ispa1ien")))
```

### Posting a form-data

You can specify a form-data at `:content` in an association list. The data will be sent in `application/x-www-form-urlencoded` format.

```common-lisp
(dex:post "http://example.com/entry/create"
          :content '(("title" . "The Truth About Lisp")
                     ("body" . "In which the truth about lisp is revealed, and some alternatives are enumerated.")))
```

### Auto-detects Multipart

If the association list contains a pathname, the data will be sent as `multipart/form-data`.

```common-lisp
(dex:post "http://example.com/entry/create"
          :content '(("photo" . #P"images/2015030201.jpg")))
```

### Following redirects (GET or HEAD)

If the server reports that the requested page has moved to a different location (indicated with a Location header and a 3XX response code), Dexador will redo the request on the new place, the fourth return value shows.

```common-lisp
(dex:head "http://lisp.org")
;=> ""
;   200
;   #<HASH-TABLE :TEST EQUAL :COUNT 7 {100D2A47A3}>
;   #<QURI.URI.HTTP:URI-HTTP http://lisp.org/index.html>
;   NIL
```

You can limit the count of redirection by specifying `:max-redirects` with an integer. The default value is `5`.

### Faking a User-Agent header

You can overwrite the default User-Agent header by simply specifying "User-Agent" in `:headers`.

```common-lisp
(dex:head "http://www.sbcl.org/" :verbose t)
;-> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;   HEAD / HTTP/1.1
;   User-Agent: Dexador/0.1 (SBCL 1.2.6); Darwin; 14.1.0
;   Host: www.sbcl.org
;   Accept: */*
;   
;   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(dex:head "http://www.sbcl.org/"
          :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18"))
          :verbose t)
;-> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;   HEAD / HTTP/1.1
;   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18
;   Host: www.sbcl.org
;   Accept: */*
;   
;   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
```

### Reusing a connection

Dexador reuses a connection by default. As it skips a TCP handshake, it would be much faster when you send requests to the same host continuously.

The interval to reuse the connection can be configured with `*reuse-interval*`. The default value is `5` (sec).

## Functions

All functions take similar arguments.

- `uri` (string or quri:uri)
- `method` (keyword)
  - The HTTP request method: `:GET`, `:HEAD`, `:OPTIONS`, `:PUT`, `:POST`, or `:DELETE`. The default is `:GET`.
- `version` (number)
  - The version of the HTTP protocol: typically `1.0` or `1.1`. The default is `1.1`.
- `content` (string, alist or pathname)
  - The body of the request.
- `headers` (alist)
  - The headers of the request. If the value of a pair is `NIL`, the header won't be sent. You can overwrite the default headers (Host, User-Agent and Accept) by this with the same header name.
- `timeout` (fixnum)
  - The seconds to timeout of the HTTP connection. The default is `10`, the value of `*default-timeout*`.
- `keep-alive` (boolean)
  - A flag if the connection keep connected even after the HTTP request. The default is `NIL`.
- `use-connection-pool` (boolean)
  - A flag if use a connection pool.
- `max-redirects` (fixnum)
  - The limit of redirections. The default is `5`. If the redirection exceeds the limit, functions return the last response (not raise a condition).
- `force-binary` (boolean)
  - A flag for suppressing auto-decoding of the response body.
- `ssl-key-file`, `ssl-cert-file`, `ssl-key-password`
  - for HTTPS connection
- `socket`
  - The socket to send an HTTP request. This is the way to reuse a connection and commonly used with `:keep-alive T`.
- `verbose` (boolean)
  - This option is for debugging. If this is `T`, it dumps the HTTP request headers.

### \[Function\] request

```common-lisp
(dex:request uri &key method version content headers timeout keep-alive max-redirects
                   ssl-key-file ssl-cert-file ssl-key-password
                   socket verbose)
;=> body
;   status
;   response-headers
;   uri
;   socket
```

Send an HTTP request to `uri`.

### \[Function\] get

```common-lisp
(dex:get uri &key version headers keep-alive timeout max-redirects force-binary
               ssl-key-file ssl-cert-file ssl-key-password
               socket verbose)
```

### \[Function\] post

```common-lisp
(dex:post uri &key version headers content keep-alive timeout force-binary
                ssl-key-file ssl-cert-file ssl-key-password
                socket verbose)
```

### \[Function\] head

```common-lisp
(dex:head uri &key version headers timeout max-redirects force-binary
                ssl-key-file ssl-cert-file ssl-key-password
                socket verbose)
```

### \[Function\] put

```common-lisp
(dex:put uri &key version headers content keep-alive timeout force-binary
               ssl-key-file ssl-cert-file ssl-key-password
               socket verbose)
```

### \[Function\] delete

```common-lisp
(dex:delete uri &key version headers keep-alive timeout force-binary
                  ssl-key-file ssl-cert-file ssl-key-password
                  socket verbose)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
