{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, AllowAmbiguousTypes          #-}

type family Dim v

type family v `OfDim` (n :: Dim v) = r | r -> n

(!*^) :: Dim m `OfDim` j -> Dim m `OfDim` i
(!*^) = undefined
