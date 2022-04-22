-- stack ghci --resolver lts-13.7 --package lens
{-# LANGUAGE RankNTypes #-}
module LensLibraryOverview
    ( Traversal, Traversal', foldMapOf, over, set, toListOf
    , Lens, Lens', lens, view
    , Prism, Prism', prism, review
    , _1, _2
    , _Left, _Right, _Nothing, _Just
    ) where

import qualified Control.Lens -- lens



----
-- Traversal
----

-- | A traversal references zero or more elements in a collection.
type Traversal s s' a a' =
    Control.Lens.Traversal s s' a a'

-- | Type alias for monomorphic traversals.
type Traversal' s a = Traversal s s a a

-- | Summarize the elements of the collection referenced by the traversal.
foldMapOf :: (Monoid r) => Traversal' s a -> (a -> r) -> (s -> r)
foldMapOf = Control.Lens.foldMapOf

-- | Map over the elements of the collection referenced by the traversal.
over :: Traversal s s' a a' -> (a -> a') -> (s -> s')
over = Control.Lens.over

-- | Set the elements of the collection referenced by the traversal.
set :: Traversal s s' a a' -> a' -> (s -> s')
set = Control.Lens.set

-- | Get the elements of the collection referenced by the traversal.
toListOf :: Traversal' s a -> (s -> [a])
toListOf = Control.Lens.toListOf



----
-- Lens
----

-- | A lens references one field in a struct.
type Lens s s' a a' =
    Control.Lens.Lens s s' a a'

-- | Type alias for monomorphic lenses.
type Lens' s a = Lens s s a a

-- | Construct a lens.
lens ::
    (s -> a) ->        -- ^ A function for getting
    (s -> a' -> s') -> -- ^ And a function for setting
    Lens s s' a a'     -- ^ Together define a lens.
lens get set =
    Control.Lens.lens get set

-- | Project the struct down to the field referenced by the lens.
view :: Lens' s a -> (s -> a)
view = Control.Lens.view

-- | A lens referencing the first coordinate of an ordered pair.
_1 :: Lens (a, z) (a', z) a a'
_1 = Control.Lens._1

-- | A lens referencing the second coordinate of an ordered pair.
_2 :: Lens (z, a) (z, a') a a'
_2 = Control.Lens._2



----
-- Prism
----

-- | A prism references one variant of a union.
type Prism s s' a a' =
    Control.Lens.Prism s s' a a'

-- | Type alias for monomorphic prisms.
type Prism' s a = Prism s s a a

-- | Construct a prism.
prism ::
    (variant' -> union') ->             -- ^ A function for upcasting
    (union -> Either union' variant) -> -- ^ And a function for matching
    Prism union union' variant variant' -- ^ Together define a prism.
prism upcast match =
    Control.Lens.prism upcast match

-- | Embed into the union the variant reference by the prism.
review :: Prism' s a -> (a -> s)
review = Control.Lens.review

-- | A prism referencing the Left variant of Either.
_Left :: Prism (Either a b) (Either a' b) a a'
_Left = Control.Lens._Left

-- | A prism referencing the Right variant of Either.
_Right :: Prism (Either a b) (Either a b') b b'
_Right = Control.Lens._Right

-- | A prism referencing the Nothing variant of Maybe.
_Nothing :: Prism' (Maybe a) ()
_Nothing = Control.Lens._Nothing

-- | A prism referencing the Just variant of Maybe.
_Just :: Prism (Maybe a) (Maybe a') a a'
_Just = Control.Lens._Just



----
-- Combinators
----

-- | Every lens is a traversal.
traversalFromLens :: Lens s s' a a' -> Traversal s s' a a'
traversalFromLens = id -- The type signature, together with the fact that the implementation is `id`, is a proof of the assertion in the doc comment.

-- | Every prism is a traversal.
traversalFromPrism :: Prism s s' a a' -> Traversal s s' a a'
traversalFromPrism = id

-- | lens × lens is a lens.
composeLensLens :: Lens z z' y y' -> Lens y y' x x' -> Lens z z' x x'
composeLensLens = (.) -- The type signature, together with the fact that the implementation is `(.)`, is a proof of the assertion in the doc comment.

-- | prism × prism is a prism.
composePrismPrism :: Prism z z' y y' -> Prism y y' x x' -> Prism z z' x x'
composePrismPrism = (.)

-- | traversal × traversal is a traversal.
composeTraversalTraversal :: Traversal z z' y y' -> Traversal y y' x x' -> Traversal z z' x x'
composeTraversalTraversal = (.)

-- | prism × lens is a traversal.
composePrismLens :: Prism z z' y y' -> Lens y y' x x' -> Traversal z z' x x'
composePrismLens = (.)

-- | lens × prism is a traversal.
composeLensPrism :: Lens z z' y y' -> Prism y y' x x' -> Traversal z z' x x'
composeLensPrism = (.)
