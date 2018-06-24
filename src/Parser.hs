module Parser
  ()
where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
--import Control.Monad.Identity (Identity)

--parse rule text = Parsec.parse rule "(source)" text
