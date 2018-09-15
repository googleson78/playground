import Options.Applicative

import qualified Data.Text as T

import Data.Semigroup ((<>))

gitRef :: Parser T.Text
gitRef = strArgument
    ( metavar "GITREF"
   <> help "A git reference."
    )

gitRefs :: Parser (T.Text, T.Text)
gitRefs = (,) <$> gitRef <*> gitRef

verbosity :: Parser T.Text
verbosity = strOption
    ( long "verbosity"
   <> short 'v'
   <> help "Possible values: full, nightly, release"
   <> metavar "VERBOSITY"
    )

fullParse :: Parser (T.Text, T.Text, T.Text)
fullParse = (,,) <$> gitRef <*> gitRef <*> verbosity

a = info fullParse fullDesc
