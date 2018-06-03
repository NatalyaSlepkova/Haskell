{-# LANGUAGE TemplateHaskell #-}

module TemplateSample
  ( Struct (..)
  ) where

import Template (genShowText)

data Struct
  = A
  | B Int
  | C { x :: Int }

genShowText ''Struct

