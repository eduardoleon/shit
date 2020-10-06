functor T1 (
  structure F : SOURCE
  structure S : SOURCE ) : SOURCE =
ZipSources (
  structure F = F
  structure S = S )

functor T2 (
  structure F : SINK
  structure S : SINK ) : SINK =
ZipSinks (
  structure F = F
  structure S = S )

functor T3 (
  structure F : SOURCE
  structure B : SOURCE where type element = F.element ) : SOURCE =
ChainSources (
  structure F = F
  structure B = B )

functor T4 (
  structure F : SINK
  structure B : SINK where type element = F.element ) : SINK =
ChainSinks (
  structure F = F
  structure B = B )

functor T5 (
  type element
  structure O : SINK
  structure I : SOURCE
  where type element = (element, O.element) Either.either ) : SOURCE =
FilterLefts (
  type element = element
  structure I = I
  structure O = O )

functor T6 (
  type element
  structure O : SINK
  structure I : SOURCE
  where type element = (O.element, element) Either.either ) : SOURCE =
FilterRights (
  type element = element
  structure I = I
  structure O = O )

functor T7 (
  structure L : SINK
  structure R : SINK ) : SINK =
ForkSinks (
  structure L = L
  structure R = R )

functor T8 (
  structure E : SOURCE
  structure I : SOURCE where type element = E.iterator
  structure L : SINK   where type element = E.leftovers ) : SOURCE =
Flatten (
  structure E = E
  structure L = L
  structure I = I )

functor T9 (
  structure I : SOURCE
  structure E : SINK   where type element = I.element
  structure L : SINK   where type element = I.leftovers ) : SINK =
Extend (
  structure E = E
  structure L = L
  structure I = I )
