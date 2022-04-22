-- stack ghci --resolver lts-13.7 --package random
{-# LANGUAGE RankNTypes, DeriveFunctor, DeriveGeneric #-}
module LensesIntroduction where

import Data.Foldable (fold) -- base
import System.IO.Unsafe -- base
import System.Random -- random

data Record a = Record {
    recordId :: Int,
    creation :: AccessLog,
    payload  :: a
} deriving (Show)

data AccessLog = AccessLog {
    user   :: String,
    moment :: Moment
} deriving (Show)

data Moment = Moment {
    date :: Date,
    time :: Time
} deriving (Show)

data Date = Date {
    year  :: Int,
    month :: Int,
    day   :: Int
} deriving (Show)

data Time = Time {
    hour   :: Int,
    minute :: Int,
    second :: Int
} deriving (Show)

-- I prefer Haskell over Javascript, so I ported my talk...
danielsLensTalk :: Record String
danielsLensTalk = Record {
    recordId = 0,
    payload  = "Daniel's Lens Talk",
    creation = AccessLog {
        user   = "danielbrice@gmail.com",
        moment = Moment {
            date = Date {
                year  = 2019,
                month = 2,
                day   = 19
            },
            time = Time {
                hour   = 18,
                minute = 30,
                second = 0
            }
        }
    }
}

-- ...and that set me back a day, so we have to fix that.
-- But GDI! This is miserable.
fixDanielsLensTalk :: Record String
fixDanielsLensTalk = danielsLensTalk {
    creation = (creation danielsLensTalk) {
        moment = ((moment . creation) danielsLensTalk) {
            date = ((date . moment . creation) danielsLensTalk) {
                day = 20
            }
        }
    }
}

-- Generalizing the pattern of setting a field isn't any worse than setting a field.
-- But that's a really low bar.
procrastinate :: Record a -> Record a
procrastinate record = record {
    creation = (creation record) {
        moment = ((moment . creation) record) {
            date = ((date . moment . creation) record) {
                day = ((day . date . moment . creation) record) + 1
            }
        }
    }
}

-- Impressions of Haskell approach to structs
--
--   1. Field access is pretty great.
--      Field accessors are ordinary functions.
--      They don't require any special syntax.
--      They compose just as easily as any other functions.
--
--   2. The niceness of field access doesn't make up for how shitty field setting is.
--      It's an unholy mess.
--      Haskell records are pretty terrible, but as bad as they are, they're not the critical issue here.
--      The issue that makes it hard is the combination of _nested data_ and _immutable data_.
--
--   3. Modifying a field (read, transform, write) in Haskell is not a whole lot harder than setting a field.
--      But, again, that's a _really_ low bar to set.
--
--
-- We need to define a `Path` data structure that will act as a reference to a location in a data structure.
-- Given a path, we need to be able to produce a getter function and a setter function.
-- Easiest thing to do: declare a `Path` to be a getter function and a setter function tupled together.

type Path struct field = (struct -> field, field -> struct -> struct)

gets :: Path struct field -> (struct -> field)
gets = fst

sets :: Path struct field -> field -> (struct -> struct)
sets = snd

-- read, transform, set
mods :: Path struct field -> (field -> field) -> (struct -> struct)
mods path f struct = sets path (f (gets path struct)) struct

-- We want to be able to trace one `Path` after another.
-- We need the end of one to be the start of the other.
-- We want to be able to think of the result as a single `Path` in its own right.
(~>) :: Path outer middle -> Path middle inner -> Path outer inner
(~>) toMiddle fromMiddle =
    let
        get z = x
            where
                y = gets toMiddle z
                x = gets fromMiddle y
        set x' z = z'
            where
                y  = gets toMiddle z
                y' = sets fromMiddle x' y
                z' = sets toMiddle y' z
    in (get, set)

-- The path to the `creation` field of `Record`
creation_ :: Path (Record a) AccessLog
creation_ = (creation, \fld str -> str { creation = fld })

-- The path to the `moment` field of `AccessLog`
moment_ :: Path AccessLog Moment
moment_ = (moment, \fld str -> str { moment = fld })

-- The path to the `date` field of `Moment`
date_ :: Path Moment Date
date_ = (date, \fld str -> str { date = fld })

-- The path to the `day` field of `Date`
day_ :: Path Date Int
day_ = (day, \fld str -> str { day = fld })

-- Now there are two ways to access nested data.
--
--   1. `gets` all the accessor functions and `.` them together.
--
--   2. `~>` all the paths together and `gets` the resulting path.
--
-- If your `Path`s are defined in the obvious way (like the examples above), these two ways will always give the same result.
creationDate :: Record a -> Date
creationDate =
    if unsafePerformIO randomIO
        then way1
        else way2
    where
        way1 = gets date_ . gets moment_ . gets creation_
        way2 = gets (creation_ ~> moment_ ~> date_)

-- Field setting is just as easy as it is with mutable data!
setCreationDate :: Date -> Record a -> Record a
setCreationDate date' record =
    sets (creation_ ~> moment_ ~> date_) date' record

-- Modifying a field with a function is just as easy!
procrastinate' :: Record a -> Record a
procrastinate' record =
    mods (creation_ ~> moment_ ~> date_ ~> day_) (+1) record

-- Impression of programming with 'Paths':
--   1. Field access is first-class, composes as paths or as functions.
--   2. Field setting is easy peasy.
--   3. Field modification is just as easy. No duplication. (So, even better than the story with mutable data!)
--   4. Paths compose easily.

-- We've solved the problems associated with nested immutable data structures.
-- Not entierly, though.
-- We've only solved the problem for specifically product-like data structures.
-- And only for monomorphic product-like data structures.
--
-- Remaining problems:
--   1. collection-like data structures
--   2. sum-like data structures
--   3. polymorphic data structures


-- Contextual Transformations

-- `validDate` takes a `Date` and gives you a `Maybe Date`.
--
-- We think of an `f Date` as a `Date` embedded in a certain context.
-- E.g., when `f ~ Maybe`, the context is uncertainty.
-- When `f ~ Writer _`, the context is history.
-- When `f ~ Reader _`, the context is sensory.
-- When `f ~ RVar`, the context is probability.
--
-- We'll call `validDate` (and similar functions) a "contextual" transformation of `Date`.
-- (N.B. Don't ever roll your own time library. It is not going to go the way you think.)
validDate :: Date -> Maybe Date
validDate date@(Date year month day) =
    let
        shortMonth =
            month `elem` [4, 6, 9, 11]
        leapYear =
            year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0)
        result
            | year < 1583                            = Nothing
            | day < 1                                = Nothing
            | month < 1                              = Nothing
            | month > 12                             = Nothing
            | month == 2 && leapYear && day > 29     = Nothing
            | month == 2 && not leapYear && day > 28 = Nothing
            | shortMonth && day > 30                 = Nothing
            | day > 31                               = Nothing
            | otherwise                              = Just date
    in result

-- We can promote a contextual transformation of the field data to a contextual transformation of the overall data structure.
modf :: (Functor ctx) => Path y x -> (x -> ctx x) -> (y -> ctx y)
modf path f y =
    let
        x = gets path y
        xs = f x
        ys = fmap (\x' -> sets path x' y) xs
    in ys

-- Validate the date of the moment of the creation of a record.
validDateRecord :: Record a -> Maybe (Record a)
validDateRecord rec =
    modf (creation_ ~> moment_ ~> date_) validDate rec

-- Print the moment of the creation of a record.
printRecordMoment :: Record a -> IO (Record a)
printRecordMoment =
    modf (creation_ ~> moment_) getPrint
    where
        getPrint x = do
            print x
            return x

-- Uncanny as it may be, it turns out that having an implementation of `modf` for particular `struct` and `field` is equivalent to having a `Path struct field`. 

type Lens' struct field =
    forall ctx. (Functor ctx) => (field -> ctx field) -> (struct -> ctx struct)

asLens :: Path struct field -> Lens' struct field
asLens path = modf path

data Identity a = Identity { unIdentity :: a } deriving Functor
data Constant x a = Constant { unConstant :: x } deriving Functor

asPath :: Lens' struct field -> Path struct field
asPath lens =
    let
        get str = unConstant (lens (\x -> Constant x) str)
        set fld str = unIdentity (lens (\_ -> Identity fld) str)
    in
    (get, set)

-- Now we can do away with `Path` altogether and use `Lens'` instead.

view' :: Lens' struct field -> (struct -> field)
view' lens = gets (asPath lens)

set' :: Lens' struct field -> field -> (struct -> struct)
set' lens = sets (asPath lens)

over' :: Lens' struct field -> (field -> field) -> (struct -> struct)
over' lens f str = unIdentity (lens (\x -> Identity (f x)) str)

-- We don't need to define an `overf` function to replace `modf`, because `overf` simply _is_ the lens itself.
--
-- overf :: (Functor ctx) => Lens' struct field -> (field -> ctx field) -> (struct -> ctx struct)
-- overf = id

_creation :: Lens' (Record a) AccessLog
_creation = asLens creation_

_moment :: Lens' AccessLog Moment
_moment = asLens moment_

_date :: Lens' Moment Date
_date = asLens date_

_day :: Lens' Date Int
_day = asLens day_

-- Lenses are functions, and they compose using ordinary function composition.
-- So we don't need to define a special operator like `~>`.

creationMoment' :: Record a -> Moment
creationMoment' =
    if unsafePerformIO randomIO
        then view' _moment . view' _creation
        else view' (_creation . _moment)

setCreationMoment' :: Moment -> Record a -> Record a
setCreationMoment' =
    set' (_creation . _moment)

procrastinate'' :: Record a -> Record a
procrastinate'' record =
    over' (_creation . _moment . _date . _day) (+1) record

validateRecordDate' :: Record a -> Maybe (Record a)
validateRecordDate' =
    (_creation . _moment . _date) validDate

printMoment' :: Record a -> IO (Record a)
printMoment' =
    (_creation . _moment) getPrint
    where
      getPrint x = do
          print x
          return x

-- Polymorphic data structures
--
-- All of the functions above work in a more generalized setting.
-- All of them can have their signatures relaxed.
-- The same implementations of `view`, `set`, and `over` all still work with no modification.

type Lens struct struct' field field' =
    forall f. Functor f => (field -> f field') -> (struct -> f struct')

view :: Lens s s' a a' -> (s -> a)
view lens str = unConstant $ lens (\x -> Constant x) str

set :: Lens s s' a a' -> a' -> (s -> s')
set lens x str = unIdentity $ lens (\_ -> Identity x) str

over :: Lens s s' a a' -> (a -> a') -> (s -> s')
over lens f str = unIdentity $ lens (\x -> Identity (f x)) str

-- The `payload` field of a record is polymorphic.
_payload :: Lens (Record a) (Record a') a a'
_payload f rec = fmap (\payload' -> rec{ payload = payload' }) (f $ payload rec)

-- The `_payload` lens captures this polymorphism and supports polymorphic record update.
attendance :: Record Int
attendance = (set _payload 15 (danielsLensTalk :: Record String) :: Record Int)

-- Collection-like Data Structures

-- A traversal is like a lens,
-- But where a lens focuses exactly one position in a data structure,
-- A tranversal simultaneously focuses zero or one or many positions in a data structure.
type Traversal s s' a a' =
    forall ctx. (Applicative ctx) => (a -> ctx a') -> (s -> ctx s')

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

-- The same definition of `set` we had for `Lens` also works for `Traversal`.
-- It will set _every_ position (so not very useful).
sett :: Traversal s s' a a' -> a' -> (s -> s')
sett trav x col = unIdentity $ trav (\_ -> Identity x) col

-- The same definition of `over` we had for `Lens` also works for `Traversal`.
-- It will apply a tranformation to the data at each position the traversal focuses.
overt :: Traversal s s' a a' -> (a -> a') -> (s -> s')
overt trav f col = unIdentity $ trav (\x -> Identity (f x)) col

instance Monoid m => Applicative (Constant m) where
    Constant m1 <*> Constant m2 = Constant (m1 <> m2)
    pure _ = Constant mempty

-- Extract the data at the positions focused by this traversal.
toListOf :: Traversal s s' a a' -> (s -> [a])
toListOf trav col = unConstant $ trav (\x -> Constant [x]) col

-- Summarize the data at the positions focused by this traversal.
foldMapOf :: Monoid m => Traversal s s' a a' -> (a -> m) -> (s -> m)
foldMapOf trav f col = unConstant $ trav (\x -> Constant (f x)) col

-- A traversal that simultaneously focuses the year, month, and day and the hour, minute, and second of the date and the time of a moment.
_momentParts :: Traversal Moment Moment Int Int
_momentParts f (Moment (Date year month day) (Time hour minute second)) =
    Moment
        <$> (Date <$> f year <*> f month <*> f day)
        <*> (Time <$> f hour <*> f minute <*> f second)

procrastinateALot :: Record a -> Record a
procrastinateALot rec = overt (_creation . _moment . _momentParts) (+1) rec

-- 4. Prisms, optic for sum-like data structures, coming some day!
