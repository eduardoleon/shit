Super Hipster ITerators
=======================

Redesign of the iterator interface to maximize their flexibility, composability
and safety.


Source
------

Step-by-step process that produces an element after each step.  If the process
is finite, the last step does not produce an element, but yields the process'
leftovers.

Categorically, an `F`-coalgebra for `F(X) = E*X + L`, where `E` and `L` are the
element and leftovers types.


Sink
----

Step-by-step process that consumes an element before each step.  If the process
is finite, the last step yields the process' leftovers.

Categorically, an `F`-coalgebra for `F(X) = E -> X+L`, where `E` and `L` are the
element and leftovers types.


Leftovers
---------

Final result of a step-by-step process.  Useful for propagating information to
the code that follows after a loop.


Universal Looping Construct
---------------------------

Every single-threaded loop arises from the interaction between a source and a
sink.  More precisely, the source feeds elements to the sink one by one, until
either the source is depleted (cannot produce more elements) or the sink is
saturated (cannot consume more elements).  If both the source and the sink are
infinite processes, then the loop is also infinite.


Implementations
---------------

Sample implementations are provided in Standard ML and Scala.  The design is not
overly dependent on any specific language features, but algebraic data types and
parametric polymorphism are a must.  ML-style modules will also make the code
nicer.


Why not Haskell?
----------------

Because lazy evaluation by default sucks.


Bugs
----

Must be in your compiler, not my code.
