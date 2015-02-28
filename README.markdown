# Dexador

Dexador is yet another HTTP client for Common Lisp.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

```common-lisp
(dex:get "http://lisp.org/")

(dex:post "https://example.com/login"
          :content '(("name" . "fukamachi") ("password" . "1ispa1ien")))
```

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
- `max-redirects` (fixnum)
  - The limit of redirections. The default is `5`. If the redirection exceeds the limit, functions return the last response (not raise a condition).
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
;   socket
```

Send an HTTP request to `uri`.

### \[Function\] get

```common-lisp
(dex:get uri &key version headers keep-alive timeout max-redirects
               ssl-key-file ssl-cert-file ssl-key-password
               socket verbose)
```

### \[Function\] post

```common-lisp
(dex:post uri &key version headers content keep-alive timeout
                ssl-key-file ssl-cert-file ssl-key-password
                socket verbose)
```

### \[Function\] head

```common-lisp
(dex:head uri &key version headers timeout max-redirects
                ssl-key-file ssl-cert-file ssl-key-password
                socket verbose)
```

### \[Function\] put

```common-lisp
(dex:put uri &key version headers content keep-alive timeout
               ssl-key-file ssl-cert-file ssl-key-password
               socket verbose)
```

### \[Function\] delete

```common-lisp
(dex:delete uri &key version headers keep-alive timeout
                  ssl-key-file ssl-cert-file ssl-key-password
                  socket verbose)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
