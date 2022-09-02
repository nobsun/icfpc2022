{-# LANGUAGE FlexibleInstances #-}
module ISL where

import Types
import Text.ParserCombinators.ReadP

rProgram :: ReadP [ProgLine]
rProgram = sepBy1 rProgramLine rNewline <* eof 

rNewline :: ReadP ProgLine
rNewline = Newline <$ char '\n'

rProgramLine :: ReadP ProgLine
rProgramLine = {- rNewLine +++ -} rComment +++ (Move <$> rMove)

rComment :: ReadP ProgLine
rComment = Comment <$> (char '#' *> munch (/= '\n'))


