{-|
Module      : Purrlude.Data.Tuple
Description : generic tuple functions using type families
License     : MIT
Stability   : experimental
Portability : POSIX
-}
--------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module Purrlude.Data.Tuple
    (
    -- * Types
      Fstable
    , Fst
    , MapFst

    , Sndable
    , Snd
    , MapSnd

    , Thdable
    , Thd
    , MapThd

    -- * Tuple Selectors
    , fst
    , snd
    , thd

    -- * Mapping Selectors
    , mapFst
    , mapSnd
    , mapThd
    )
    where
--------------------------------------------------------------------------------
import           Prelude            hiding (fst, snd)
--------------------------------------------------------------------------------

{-
fundeps and typefamilies both work pretty well here. i opted for typefamilies
for no reason in particular.

    -------- With TypeFamilies --------
    class Sndable a where
        type Snd a
        snd :: a -> Snd a

    instance Sndable (a, b) where
        type Snd (a, b) = b
        snd (_,b) = b

    -------- With FunctionalDependencies --------
    class Sndable a s | a -> s where
        snd :: a -> s
    
    instance Sndable (a, b) b where
        snd (_,b) = b

update after adding the 'mapSnd' method: type families work way better lmfao.
-}

-- TODO: template haskell.. lol..

class Fstable a where
    type Fst a
    fst    :: a -> Fst a

    type MapFst a a'
    mapFst :: (Fst a -> a') -> a -> MapFst a a'

instance Fstable (a, b) where
    type Fst (a, b) = a
    fst (a,_) = a

    type MapFst (a, b) a' = (a', b)
    mapFst f (a, b) = (f a, b)

instance Fstable (a, b, c) where
    type Fst (a, b, c) = a
    fst (a,_,_) = a

    type MapFst (a, b, c) a' = (a', b, c)
    mapFst f (a,b,c) = (f a, b, c)

instance Fstable (a, b, c, d) where
    type Fst (a, b, c, d) = a
    fst (a,_,_,_) = a

    type MapFst (a, b, c, d) a' = (a', b, c, d)
    mapFst f (a,b,c,d) = (f a, b, c, d)

--------------------------------------------------------------------------------

class Sndable a where
    type Snd a
    snd :: a -> Snd a

    type MapSnd a a'
    mapSnd :: (Snd a -> a') -> a -> MapSnd a a'

instance Sndable (a, b) where
    type Snd (a, b) = b
    snd (_,b) = b

    type MapSnd (a, b) b' = (a, b')
    mapSnd f (a, b) = (a, f b)

instance Sndable (a, b, c) where
    type Snd (a, b, c) = b
    snd (_,b,_) = b

    type MapSnd (a, b, c) b' = (a, b', c)
    mapSnd f (a, b, c) = (a, f b, c)

instance Sndable (a, b, c, d) where
    type Snd (a, b, c, d) = b
    snd (_,b,_,_) = b

    type MapSnd (a, b, c, d) b' = (a, b', c, d)
    mapSnd f (a, b, c, d) = (a, f b, c, d)

--------------------------------------------------------------------------------

class Thdable a where
    type Thd a
    thd :: a -> Thd a

    type MapThd a a'
    mapThd :: (Thd a -> a') -> a -> MapThd a a'

instance Thdable (a, b, c) where
    type Thd (a, b, c) = c
    thd (_,_,c) = c

    type MapThd (a, b, c) c' = (a, b, c')
    mapThd f (a,b,c) = (a,b,f c)

instance Thdable (a, b, c, d) where
    type Thd (a, b, c, d) = c
    thd (_,_,c,_) = c

    type MapThd (a, b, c, d) c' = (a, b, c', d)
    mapThd f (a,b,c,d) = (a,b,f c,d)

