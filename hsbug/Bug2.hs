{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

f = undefined :: (forall (a :: k) m. m a -> Int) -> Int
