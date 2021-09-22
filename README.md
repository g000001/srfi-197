# SRFI 197 for CL: Pipeline Operators

A Scheme library that implements `chain`, an operator based on [Clojure
threading macros][1]. This is the sample implementation of [SRFI 197][2].

[1]: https://clojure.org/guides/threading_macros
[2]: https://srfi.schemers.org/srfi-197/srfi-197.html

```
Test group: Pipeline Operators

PASS: chain
PASS: chain with mixed _ position
PASS: chain with _ in operator position
PASS: chain without _
PASS: chain _ ...
  SKIP: chain with custom _
  SKIP: chain with custom ...
PASS: chain-and
PASS: chain-and with mixed _ position
PASS: chain-and without _
PASS: chain-and short-circuit
PASS: chain-and short-circuit first
  SKIP: chain-and with custom _
PASS: chain-when
  SKIP: chain-when with mixed _ position
PASS: chain-when without _
  SKIP: chain-when with custom _
PASS: chain-lambda
PASS: chain-lambda one step
PASS: chain-lambda multiple _
PASS: chain-lambda without _
  SKIP: chain-lambda _ ...
  SKIP: chain-lambda _ _ ...
  SKIP: chain-lambda with custom _
  SKIP: chain-lambda with custom ...
PASS: nest
PASS: nest with custom _
PASS: nested nest
PASS: nest-reverse
PASS: nest-reverse with custom _
```
