# Dexador

Dexador is yet another HTTP client for Common Lisp.

## Usage

```common-lisp
(dex:get "http://lisp.org/")
```

## API

### \[Function\] (request uri &key method version content headers keep-alive socket)

Send an HTTP request to `uri`.

### \[Function\] (get uri &key version headers keep-alive socket)

### \[Function\] (post uri &key version headers content keep-alive socket)

### \[Function\] (head uri &key version headers socket)

### \[Function\] (put uri &key version headers content keep-alive socket)

### \[Function\] (delete uri &key version headers keep-alive socket)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
