# (Functional) References #


## Further Learning ##

1. Simon Peyton Jones, "Lenses: compositional data access and manipulation" (https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation).

2. Brian McKenna, "Productionisation of Functional Optics" (https://www.youtube.com/watch?v=H01dw-BMmlE).

3. Gabrial Gonzalez, "lens-tutorial" (http://hackage.haskell.org/package/lens-tutorial).


## Notable Haskell Lens Libraries ##

1. Edward Kmett, "lens" (https://hackage.haskell.org/package/lens).

  This is the definitive, all-inclusive optics library in Haskell. Includes lenses, traversals, prisms, folds, isos, etc. It exports the `Control.Lens` module and Template Haskell for generating lenses and traversals for your custom data types.

2. Russell O'Connor, Michael Thompson, "lens-simple" (http://hackage.haskell.org/package/lens-simple).

  Adapted from "lens-family", which predates "lens" by a few weeks. Includes lenses and traversals while seeking to minimize conceptual overhead/complexity.

3. Edward Kmett, Artyom Kazak, "microlens" (http://hackage.haskell.org/package/microlens).

  Adapted from "lens", includes lenses and traversals while seeking to minimize dependencies.

4. Csongor Kiss, "generic-lens" (http://hackage.haskell.org/package/generic-lens).

  Provides lenses, traversals, and prisms for custom data types, without Template Haskell, provided the custom data type has has a `Generic` instance.


## Lens Libraries in Other Languages ##

1. Gary Burgess, Phil Freeman, Liam Goodacre, "purescript-profunctor-lenses" (https://pursuit.purescript.org/packages/purescript-profunctor-lenses/).

  Purescript implementation of profunctor optics.

2. Naoki Aoyama, Ilan Godik, Ken Scambler, Julien Truffaut, Kenji Yoshida, "Monocle" (http://julien-truffaut.github.io/Monocle/).

  Scala implementation of optics hierarchy inspired by Haskell's "lens" library.

3. Nathan Adams, Philip J. Fry, "DataFixerUpper" (https://github.com/Mojang/DataFixerUpper/tree/master/src/main/java/com/mojang/datafixers/optics).

  Java implementation of profunctor optics.


## History of Functional References/Lenses ##

The "lens" library comes with a "History of Lenses" page. Here are a few notable entries and a few additions.

1. 2007, Luke Palmer, "Making Haskell nicer for game programming" (http://web.archive.org/web/20080515203207/http://luqui.org/blog/archives/2007/07/26/making-haskell-nicer-for-game-programming/).

  Palmer describes the well-known problem of setting/updating nested fields in an immutable data structure and introduces the `Accessor` data type (similar to our `Path` type) as a proposed solution. He devises a pre-processor step to avoid the boilerplate of defining `Accessor`s and describes a small monadic DSL for data access and manipulation based on `Accessor`s.

2. 2008, Edward A. Kmett, Russell O'Connor, Tony Morris, "data-lens" (http://hackage.haskell.org/package/data-lens).

  Takes Palmer's ideas and build them into a production-ready Haskell abstraction. The (monomorphic) `Lens a b` type is a `newtype` based on the `Store` comonad, which is related to the familiar `State` monad and (when composed with a Reader monad) encapsulates the notion of having a getter and a setter.

3. 2008, Conal Elliott, "Semantic editor combinators" (http://conal.net/blog/posts/semantic-editor-combinators).

  By what amounts to giving individual names to specific functors (e.g. giving `fmap` for `(t,)` the name `second`, giving `fmap` for `(t->)` the name `result`, giving `contramap` for `(->t)` the name `argument`, and defining an analog to `fmap` for `(,t)` named `first`), Elliott shows that data manipulation can be made compositional and polymorphic (Conel's "semantic editor combinators" are the same signature as the `over` function applied to a lens). He then draws a comparison to the `Arrow` class.

4. 2009, Twan van Laarhoven, "CPS based functional references" (https://www.twanvl.nl/blog/haskell/cps-functional-references).

  Van Laarhoven describes the modern encoding of (monomorphic) lenses, i.e. as a function that lifts an effectful computation on the field to an effectful computation on the struct. Advantages over previous encodings include performance (no data types involved, just ordinary functions) and ease of composition (describe a path into a data structure using ordinary function composition).

5. 2012, Russell O'Conner, "Polymorphic Update with van Laarhoven Lenses" (http://r6.ca/blog/20120623T104901Z.html).

  O'Conner points out that by adding type parameters, van Laarhoven lens can be made polymorphic, bridging the gap between functional references and Conal Elliott's semantic editor combinators.

6. 2012, Edward Kmett, "Mirrored Lenses" (http://comonad.com/reader/2012/mirrored-lenses/).

  Kmett points out that Elliott's polymorphic lenses no longer satisfy the common-sense lens laws if the four type parameters are allowed to vary independently. Kmett coins the term "lens family" to describe these generalized lens-like constructs. He then goes on to describe more generalized lens-like constructs (`Getter`s, `Setter`s, and `Modifier`s), laying the ground work for the optics hierarchy we have today.
