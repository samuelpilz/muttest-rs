# Mutations

## Expr & Stmt Mutations

* Literal
  * integer/float
  * string/byte/char
  * bool
* comparison operator
  * change direction
  * change bounds
  * change to eq/ne
* arithmetic Operator
  * change binop
  * change/remove unop
  * change assignOp
* assign
  * remove assign
  * re-target assign
* path
  * change path
  * change variable read
  * change field read
* match
  * remove arm
  * remove guard
  * clear arms
* loops
  * ??
* if
  * remove else
  * remove condition
* range
  * mutate kinds (difficult to implement)
* return
  * remove return
* block
  * clear block
  * remove statement
* edit break/continue label

