# Nodes
==========

## About
Nodes is a concurrent programming language, based on the Pi-Calculus. It is
implemented in scala, both sequentially and concurrently. The sequential
implementation is based on the algorithm given in David N. Turner's PhD thesis,
while the concurrent implementation is a true-to-semantics implementation using 
Akka, scala's concurrency library. 
==========

## State
At present, the project is in a pre-alpha state. A fairly solid lexer and parser
have been built using JFlex and CUP, with nice error messages.
The implementation uses dynamic typing, with no attempt to give nice error
messages.
==========

## Todos
The main priorities for this project are:
- To implement a static type system
- To implement more sophisticated data types, with ML style type inference
- To provide a nicer command-line interface, allowing the user to specify
  which implementation to use
