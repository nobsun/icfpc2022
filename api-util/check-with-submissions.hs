import System.Environment (getArgs)
import ApiUtil (checkCostsWithSubmissions)

main :: IO ()
main = do
  args <- getArgs
  engine <- case args of
    []   -> return ""
    x:_  -> return x
  checkCostsWithSubmissions engine
