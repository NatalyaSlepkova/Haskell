{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Block2 where
import           Data.List
import           Data.Maybe
import           Text.Read
import           Data.Monoid (Monoid)

stringSum :: String -> Maybe Int
stringSum str = sum <$> sequence (readMaybe <$> words str)


data Optional a = Optional (Maybe (Maybe a)) deriving (Show, Eq)

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap _ (Optional Nothing)         = Optional Nothing
    fmap _ (Optional (Just Nothing))  = Optional (Just (Nothing))
    fmap f (Optional (Just (Just a))) = Optional (Just (Just (f a)))

instance Applicative Optional where
    pure :: a -> Optional a
    pure a = Optional (Just (Just a))
    (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    _ <*> Optional (Just (Nothing))                       = Optional (Just (Nothing))
    Optional (Just (Nothing)) <*> _                       = Optional (Just (Nothing))
    Optional (Just (Just f)) <*> Optional (Just (Just y)) = Optional (Just (Just (f y)))
    _ <*> _ = Optional Nothing
 
instance Monad Optional where 
    return :: a -> Optional a
    return = pure
    (>>=) :: Optional a ->  (a -> Optional b) -> Optional b
    Optional (Just Nothing) >>= _  = Optional (Just Nothing)
    Optional (Just (Just a)) >>= f = f a
    _ >>= _                        = Optional Nothing 

instance Foldable Optional where
    foldMap :: Monoid m => (a -> m) -> Optional a -> m
    foldMap f (Optional (Just (Just x))) = f x
    foldMap _ _                          = mempty

instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse _ (Optional Nothing)          = pure (Optional Nothing)
    traverse _ (Optional (Just (Nothing))) = pure (Optional (Just (Nothing)))
    traverse f (Optional (Just (Just x)))  = fmap pure (f x)


data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Functor NonEmpty where
    fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
    fmap f (a:| xs) = f a :| fmap f xs
 
instance Applicative NonEmpty where
    pure :: a -> NonEmpty a
    pure a = a :| []
    (x :| xtl) <*> (y :| ytl) =  x y :| drop 1 [x2 y2 | x2 <- x:xtl, y2 <- y:ytl]

instance Monad NonEmpty where
    (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    (>>=) (a :| tl) f = f a
    return a          = a :| []

instance Foldable NonEmpty where
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f acc (a :| tl) = f a (foldr f acc tl)
 
instance Traversable NonEmpty where
    traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    traverse f (a :| tl) = fmap (:|) (f a) <*> traverse f tl