import Data.Semigroup (Endo(..), (<>))
import Data.Coerce (coerce)

import qualified Data.Text as T

chain :: [a -> a] -> (a -> a)
chain = appEndo . mconcat . coerce
