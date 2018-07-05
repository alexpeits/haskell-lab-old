module MonadAdventures where

import Control.Monad.Logic
import Control.Monad

choices :: MonadPlus m => [a] -> m a
choices = msum . map return

fairIntegers :: Logic Integer
fairIntegers = return 0 `mplus`
  (choices [1..] `interleave` choices [-1,-2..])
