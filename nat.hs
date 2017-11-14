module Nat where

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number 7"
lucky _ = "UNLUCKY"
