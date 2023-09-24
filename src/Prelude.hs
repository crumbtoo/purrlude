{-# LANGUAGE PackageImports #-}
module Prelude
    ( id
    , fmap
    , undefined

    -- common operators
    , (&&)
    , (||)
    , (++)
    , (.)
    , ($)
    , (<*>)
    , (>>=)
    , (>>)
    , pure

    -- types
    , Int
    , Integer
    , Float
    , Double
    , Rational
    , Maybe(..)
    , Bool(..)

    , Sum(..)
    , Product(..)

    -- typeclasses
    , Num
    , Eq
    , Show
    )
    where
--------------------------------------------------------------------------------
import "base" Prelude
import Data.Monoid      (Sum(..), Product(..))

