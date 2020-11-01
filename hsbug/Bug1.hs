{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

import GHC.Exts

type family MatchInt (f :: Int) :: () where
  MatchInt ('I# _) = '()
