{-# LANGUAGE TemplateHaskell #-}
module TestTemplateHask where

import TemplateHask (precompute, bigBadMathProblem)

$(precompute [1..10])
