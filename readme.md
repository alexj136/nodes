# Nodes

## About

Nodes is a concurrent programming language, based on the Pi-Calculus. It is
implemented in scala, both sequentially and concurrently. The sequential
implementation is based on the algorithm given in David N. Turner's PhD thesis,
while the concurrent implementation is a true-to-semantics implementation using 
Akka, scala's concurrency library. 

## Source language syntax

#### Processes

    P ::= send E ; TS ; ES . P          Output
        | receive E ; XS ; AS . P       Input
        | server E ; XS ; AS . P        Replicated input
        | let X : T = E . P             Local abstraction
        | if E then P1 else P2 endif    Conditional
        | [ P1 | P2 | ... | PN ]        N-ary parallel composition
        | new X . P                     Channel creation
        | end                           Null process

    XS ::= ε                            Name list
        | X, XS

    TS ::= ε                            Type list
        | T, TS

    ES ::= ε                            Expression list
        | E, ES

    AS ::= ε                            Typed argument list
        | X: T, AS

#### Expressions

    E, F ::= X                          Variables ([a-z_]+)
        | C                             Channel literals ($[a-z_]+)
        | I                             Integers (0|[1-9][0-9]*)
        | 'a'                           Character literals
        | "abc"                         String literals
        | true | false                  Booleans
        | E + F                         Addition
        | E - F                         Subtraction
        | E * F                         Multiplucation
        | E / F                         Integer division
        | E % F                         Modulo (remainder after integer division)
        | E == F                        Equality
        | E != F                        Inequality
        | E < F                         Less-than
        | E <= F                        Less-or-equal
        | E > F                         Greater-than
        | E >= F                        Greater-or-equal
        | E && F                        Logical and
        | E || F                        Logical or
        | ! E                           Logical not
        | { E , F }                     Tuple constructor
        | <- E                          Left-hand tuple destructor
        | -> E                          Right-hand tuple destructor
        | [ E1 , E2, ... , EN ]         List literals
        | []                            Empty list
        | ? E                           Is-empty list
        | *-- E                         Head of list
        | -** E                         Tail of list
        | E :: F                        List constructor
        | (E)                           Parenthesised expression

#### Types

    T, U ::= int                        Integer
        | bool                          Boolean
        | char                          Char
        | string                        String (equivalent to [ char ])
        | [ T ]                         List
        | { T , U }                     Pair
        | @T                            Channel
        | X                             Type Variable

## Language semantics

TODO

## Dependencies
Just SBT and a JRE8. SBT will download the required scala compiler and
libraries.
