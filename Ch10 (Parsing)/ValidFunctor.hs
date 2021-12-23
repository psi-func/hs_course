{-# LANGUAGE DatatypeContexts #-}

module ValidFunctor where

    data (Eq a) => Bar a = Bar a

    data Foo a = Foo a

    instance Functor Foo where
        fmap f (Foo a) = Foo (f a)
    
    
    instance Functor Bar where
        fmap f (Bar a) = Bar (f a)

-- NOT good idea


    

    