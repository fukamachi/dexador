# Dexador

Dexador is yet another HTTP client for Common Lisp.

## Usage

```common-lisp
(dex:get "http://lisp.org/")
```

## API

### \[Function\] (request uri &key method version socket keep-alive)

Send an HTTP request to `uri`.

### \[Function\] (get uri &key version socket keep-alive)

### \[Function\] (post uri &key version socket keep-alive)

### \[Function\] (head uri &key version socket)

### \[Function\] (put uri &key version socket keep-alive)

### \[Function\] (delete uri &key version socket keep-alive)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
