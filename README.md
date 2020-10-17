# sajson

[![Build Status](https://travis-ci.com/kccqzy/haskell-sajson.svg?branch=master)](https://travis-ci.com/kccqzy/haskell-sajson)

This is a Haskell package that is a thin wrapper over the wonderful
[sajson](https://github.com/chadaustin/sajson) library written by Chad Austin.
It provides a high performance JSON parser written in C++11. After parsing, a
Haskell `Data.Aeson.Value` is constructed.

## Who This Is For

This library is specifically designed for Haskell users who find the performance
of the pure-Haskell `Data.Aeson` parser inadequate. This library focuses on
performance, not portability or absolute correctness. You should *only* use this
library if all of the following is true:

*  You are using a system that is 64-bit. This library assumes
   `sizeof(size_t) == 8`. If this is not the case, the behavior is
   undefined.

*  You are using a system where the alignment of `double` is no stricter than
   that of `size_t`. Internally, the library casts a pointer to `size_t` into a
   pointer to `double` and expects this to be meaningful.

*  You do not expect to parse floating point numbers beyond the range of
   `double`.

*  You do not expect to parse integers that are longer than 53 bits.

*  You do not expect parsing a JSON number will produce a Haskell `Scientific`
   value that exactly represents the mathematical value of the JSON number, or
   even the closest approximation in IEEE-754 `double`.

   For example if you parse numbers that would be denormal numbers
   in IEEE-754 double-precision numbers, you will very likely get back zero
   instead.

   As another example, if you parse `0.3`, which cannot be represented
   exactly in IEEE-754 `double`, you will get back 0.30000000000000004, instead
   of the closest number in IEEE-754 `double` which is 0.29999999999999999.

*  You wish to sacrifice memory consumption for speed. The *intermediate* memory
   needed is 9 times the number of bytes of the input `ByteString` plus
   constant. This excludes the memory needed by the parsed `Value` or the memory
   of the input `ByteString`.

*  You are parsing JSON that is known to be valid. As a library written in C++,
   it is inherently unsafe. Despite the use of address sanitizer and AFL which
   have
   [caught memory usage bugs](https://github.com/chadaustin/sajson/commit/0bd8a661421fc3e61262c283543689b0f8a88483),
   it is difficult to guarantee that creatively crafted input will not crash or
   cause arbitrary code execution. It is recommend that you use this library
   only for parsing known good JSON.

If you satisfy all of the above, then this library could work for you. Again,
please benchmark and show that JSON parsing is indeed a bottleneck before using
this library.

## Ideas and Future Plan

*  Skip creating `Value`s when the you eventually want another Haskell type.
   Currently the `FromJSON` instance needs a `Value`, but perhaps we can devise
   another method to avoid the generation of these intermediate `Value`s.

*  Provide a benchmark so that we can quantitatively argue this library parses
   JSON faster than `Data.Aeson`.

*  Provide an alternative for parsing floating point numbers that better
   preserves the value.

*  More complete tests, including QuickCheck-based tests.
