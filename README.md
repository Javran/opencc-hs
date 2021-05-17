# opencc-hs

[中文版](./README.zh.md)

This library contains both high-level and low-level bindings to OpenCC, Open Chinese Convert. This library is an experiment on Haskell FFI and is not intended for industrial use (at least for now). But since this library is extremely simple, you are welcome to review the code and decide for yourself.

## Comparison

[hopencc](https://hackage.haskell.org/package/hopencc) is an existing Haskell library that does the same thing, but that library only provides the IO-flavor binding here. hopencc copies the data, while opencc-hs doesn't (taking advantage of the internal representation of bytestrings).
