-- stack ghci --resolver lts-13.7 --package random
{-# LANGUAGE RankNTypes, DeriveFunctor, DeriveGeneric, UnicodeSyntax #-}
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
--   1. Field access is pretty great. They're just ordinary first-class
--      functions that don't require any special syntax and compose
--      just like any other functions.
--   2. The niceness of field access doesn't make up for how shitty
--      field setting is. It's an unholy mess.
--   3. The jump from field setting to field modification in Haskell
--      is not any worse than the jump from field setting to field
--      modification in your everyday mutable lang.

type Path struct field = (struct -> field, field -> struct -> struct)

gets :: Path struct field -> (struct -> field)
gets = fst

sets :: Path struct field -> field -> (struct -> struct)
sets = snd

mods :: Path struct field -> (field -> field) -> (struct -> struct)
mods path f struct = sets path (f (gets path struct)) struct

(~>) :: Path z y -> Path y x -> Path z x
(~>) outer inner =
    let
        get z = x
            where
                y = gets outer z
                x = gets inner y
        set x' z = z'
            where
                y  = gets outer z
                y' = sets inner x' y
                z' = sets outer y' z
    in (get, set)

creation_ :: Path (Record a) AccessLog
creation_ = (creation, \fld str -> str { creation = fld })

moment_ :: Path AccessLog Moment
moment_ = (moment, \fld str -> str { moment = fld })

date_ :: Path Moment Date
date_ = (date, \fld str -> str { date = fld })

day_ :: Path Date Int
day_ = (day, \fld str -> str { day = fld })

-- Field access is first class, implemented as either function
-- composition or path composition (equivalently).
creationDate :: Record a -> Date
creationDate =
    if unsafePerformIO randomIO
        then gets date_ . gets moment_ . gets creation_
        else gets (creation_ ~> moment_ ~> date_)

-- Field setting is just as easy as in mutable langs!
setCreationDate :: Date -> Record a -> Record a
setCreationDate date' record =
    sets (creation_ ~> moment_ ~> date_) date' record

-- Modifying a field with a function is just as easy!
procrastinate' :: Record a -> Record a
procrastinate' record =
    mods (creation_ ~> moment_ ~> date_ ~> day_) (+1) record

-- Impression of programming with 'Paths':
--   1. Field access is first-class, composes as paths or as functions.
--   2. Field setting is natural.
--   3. Field modification is as natural as field setting. No duplication.
--   4. Paths compose effortlessly.

-- Remaining problems:
--   1. effectful functions
--   2. polymorphism
--   3. collection-like data structures
--   4. sum-like data structures

-- 1. Effectful Functions

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

modf :: (Functor f) => Path y x -> (x -> f x) -> (y -> f y)
modf path f y =
    let
        x = gets path y
        fx = f x
        fy = fmap (\x' -> sets path x' y) fx
    in fy

validDateRecord :: Record a -> Maybe (Record a)
validDateRecord rec =
    modf (creation_ ~> moment_ ~> date_) validDate rec

printRecordMoment :: Record a -> IO (Record a)
printRecordMoment =
    modf (creation_ ~> moment_) (\moment -> print moment >> return moment)

-- Having an implementation of `modf` is equivalent to having a `Path`.

type Lens' struct field =
    forall f. (Functor f) => (field -> f field) -> (struct -> f struct)

lensFromPath :: Path struct field -> Lens' struct field
lensFromPath path = modf path

data Identity a = Identity { unIdentity :: a } deriving Functor
data Constant x a = Constant { unConstant :: x } deriving Functor

pathFromLens :: Lens' struct field -> Path struct field
pathFromLens lens =
    let
        get str = unConstant (lens (\x -> Constant x) str)
        set fld str = unIdentity (lens (\_ -> Identity fld) str)
    in
    (get, set)

-- Now we can do away with `Path` altogether and use `Lens'` instead.

view' :: Lens' struct field -> (struct -> field)
view' lens = gets (pathFromLens lens)

set' :: Lens' struct field -> field -> (struct -> struct)
set' lens = sets (pathFromLens lens)

over' :: Lens' struct field -> (field -> field) -> (struct -> struct)
over' lens f str = unIdentity (lens (\x -> Identity (f x)) str)

-- we don't need `overf` because it simply _is_ the lens
-- overf :: (Functor f) => Lens' struct field -> (field -> f field) -> (struct -> f struct)
-- overf = id

_creation :: Lens' (Record a) AccessLog
_creation = lensFromPath creation_

_moment :: Lens' AccessLog Moment
_moment = lensFromPath moment_

_date :: Lens' Moment Date
_date = lensFromPath date_

_day :: Lens' Date Int
_day = lensFromPath day_

-- Lenses are functions, and they compose using ordinary function composition

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
    (_creation . _moment) (\moment -> print moment >> return moment)

-- 2. Polymorphic Lenses

type Lens struct struct' field field' =
    forall f. Functor f => (field -> f field') -> (struct -> f struct')

view :: Lens s s' a a' -> (s -> a)
view lens str = unConstant $ lens (\x -> Constant x) str

set :: Lens s s' a a' -> a' -> (s -> s')
set lens x str = unIdentity $ lens (\_ -> Identity x) str

over :: Lens s s' a a' -> (a -> a') -> (s -> s')
over lens f str = unIdentity $ lens (\x -> Identity (f x)) str

_payload :: Lens (Record a) (Record a') a a'
_payload f rec = fmap (\payload' -> rec{ payload = payload' }) (f $ payload rec)

attendance :: Record Int
attendance = set _payload 15 danielsLensTalk

-- 3. "multi-focused lenses" into collection-like structures

type Traversal s s' a a' =
    forall f. Applicative f => (a -> f a') -> (s -> f s')

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

sett :: Traversal s s' a a' -> a' -> (s -> s')
sett trav x col = unIdentity $ trav (\_ -> Identity x) col

overt :: Traversal s s' a a' -> (a -> a') -> (s -> s')
overt trav f col = unIdentity $ trav (\x -> Identity (f x)) col

instance Monoid m => Applicative (Constant m) where
    Constant m1 <*> Constant m2 = Constant (m1 <> m2)
    pure _ = Constant mempty

toListOf :: Traversal s s' a a' -> (s -> [a])
toListOf trav col = unConstant $ trav (\x -> Constant [x]) col

foldMapOf :: Monoid m => Traversal s s' a a' -> (a -> m) -> (s -> m)
foldMapOf trav f col = unConstant $ trav (\x -> Constant (f x)) col

_momentParts :: Traversal Moment Moment Int Int
_momentParts f (Moment (Date year month day) (Time hour minute second)) =
    Moment
        <$> (Date <$> f year <*> f month <*> f day)
        <*> (Time <$> f hour <*> f minute <*> f second)

procrastinateALot :: Record a -> Record a
procrastinateALot rec = overt (_creation . _moment . _momentParts) (+1) rec

-- 4. Prisms, optic for sum-like data structures, coming up next!
