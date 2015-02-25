# Dexador

Dexador is yet another HTTP client for Common Lisp.

## Usage

```common-lisp
(dex:get "http://lisp.org/")
```

## API

### \[Function\] (request uri &key method version content headers timeout keep-alive socket verbose)

Send an HTTP request to `uri`.

### \[Function\] (get uri &key version headers keep-alive timeout socket verbose)

### \[Function\] (post uri &key version headers content keep-alive timeout socket verbose)

### \[Function\] (head uri &key version headers timeout socket verbose)

### \[Function\] (put uri &key version headers content keep-alive timeout socket verbose)

### \[Function\] (delete uri &key version headers keep-alive timeout socket verbose)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
