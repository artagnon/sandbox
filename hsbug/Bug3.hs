{-# LANGUAGE PolyKinds #-}

data Proxy a = Proxy

class Foo (t :: k) where foo :: Proxy (a :: t)
