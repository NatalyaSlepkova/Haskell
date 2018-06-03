{-# LANGUAGE RankNTypes #-}

module Lens
  ( Lens
  , Lens'
  , setSimple
  , viewSimple
  , overSimple
  , _1
  , _2
  , lens
  , view
  , set
  , (^.)
  , (.~)
  , (%~)
  , over
  , choosing
  , (<%~)
  , (<<%~)
  ) where

import           Data.Either           (Either (..))
import           Data.Functor.Const    (Const (..), getConst)
import           Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' obj field  = Lens obj obj field field

setSimple :: Lens' obj field -> field -> obj -> obj
setSimple len f o = runIdentity $ len (const $ Identity f) o

viewSimple :: Lens' obj field -> obj -> field
viewSimple len o = getConst $ len Const o

overSimple :: Lens' obj field -> (field -> field) -> obj -> obj
overSimple len ff o = runIdentity $ len (Identity . ff) o


_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = fmap (\b -> (b, x)) (f a)

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = fmap (\b -> (x, b)) (f a)


lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = fmap (setter s) (f (getter s))


view :: Lens s t a b -> s -> a
view len s = getConst $ len Const s

set :: Lens s t a b -> s -> b -> t
set len s b = over len (const b) s

over :: Lens s t a b -> (a -> b) -> s -> t
over len f s = runIdentity $ len (Identity . f) s

(^.) :: Lens s t a b -> s -> a
(^.) = view

(.~) :: Lens s t a b -> b -> s -> t
(.~) l f o = runIdentity $ l (const $ Identity f) o

(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over

choosingGetter :: Lens s1 t1 a b -> Lens s2 t2 a b -> Either s1 s2 -> a
choosingGetter len1 _ (Left os1)  = view len1 os1
choosingGetter _ len2 (Right os2) = view len2 os2

choosingSetter :: Lens s1 t1 a b -> Lens s2 t2 a b -> Either s1 s2 -> b -> Either t1 t2
choosingSetter len1 _ (Left os1) b  = Left $ set len1 os1 b
choosingSetter _ len2 (Right os2) b = Right $ set len2 os2 b

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing len1 len2 = lens (choosingGetter len1 len2) (choosingSetter len1 len2)


(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) len f s = (f $ view len s, over len f s)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) len f s = (view len s, over len f s)

