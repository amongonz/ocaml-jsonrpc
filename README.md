# JSON-RPC codecs and handlers

This OCaml library implements [Jsont][] codecs for the JSON-RPC 2.0
specification, extended with peer-to-peer support, as well as a
request handler abstraction.

[Jsont]: https://erratique.ch/software/jsont

The current API is experimental and might go through breaking changes in
the future.

This library isn't published on opam yet, but you can give it a try by
pinning it to an opam switch:

```sh
opam pin amongonz-jsonrpc git+https://github.com/amongonz/ocaml-jsonrpc
```

Then add `amongonz-jsonrpc` as a dependency to access the `Jsonrpc`
module.
