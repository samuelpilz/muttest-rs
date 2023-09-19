# Mutations

## Expr & Stmt Mutations

* Literal
  * integer/float
  * string/byte/char
  * bool
* logic
  * equality
  * comparison
  * negation
* arithmetic Operator
  * change op
  * remove negation
  * const one side
  * change assignOp
* assign
  * remove assign
  * re-target assign
* match
  * remove arm
  * remove guard
* if
  * remove else
  * remove condition
  * negate condition
* while loop
  * convert to if
* slice
  * remove entry (only possible for type `&[T]`)
* loop
  * make into block (when no break included)
  * insert break at end (?)

## maybe also possible

* path
  * change path
  * change variable read
  * change field read
* loops
* range
  * mutate kinds (difficult to implement)
* return
  * remove return
* block
  * clear block
  * remove statement
* edit break/continue label
* let
  * remove shadow

