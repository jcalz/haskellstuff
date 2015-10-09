{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ShowAll where

class ShowAll a where
    showAll :: a -> String
    showAll _ = "unshowable"

instance (Show a) => ShowAll a where
    showAll = show
