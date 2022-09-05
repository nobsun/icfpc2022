
module ApiUtil where

import Control.Monad ((<=<), when)
import Data.List (isPrefixOf)
import System.FilePath ((</>), (<.>), takeFileName)
import System.Directory (doesFileExist)
import System.Process (rawSystem)
import System.Exit (ExitCode (..))

import Codec.Picture (DynamicImage (..), readImage)

import Types (InitialConfig (..), loadInitialConfig, defaultInitialConfig, loadISL, similarity)
import ApiJSON (Problem (..), loadProblems, Submission1 (..), loadSubmissions, Submission (..), loadSubmission)
import qualified EvalContent as Content
import qualified EvalJuicyPixels as JuicyPixels


saveSubmissions :: IO ()
saveSubmissions = do
  let submissionList = "lists/submissions.json"
  executeCmd "./api/update-submissions-list.sh" [submissionList]
  saveSubmissions_ submissionList

saveSubmissions_ :: FilePath -> IO ()
saveSubmissions_ = either fail (mapM_ saveSubmission) <=< loadSubmissions

saveSubmission :: Submission1 -> IO ()
saveSubmission si1 = do
  executeCmd "./api/save-submission-info.sh" [idstr, subInfo]
  esi <- loadSubmission subInfo
  either (const $ return ()) saveSubmit esi
  return ()
  where
    idstr = show $ sub1_id si1
    infoDir
      | sub1_status si1 == "SUCCEEDED"  =  "subs"
      | otherwise                       =  "subs" </> "err"
    subInfo = infoDir </> idstr <.> "info" <.> "json"
    saveSubmit si
      | sub_id si /= sub1_id si1 =
        putStrLn $ "submission-id inconsistent. skipping: " ++ show (sub_id si) ++ " /= " ++ show (sub1_id si1)
      | otherwise     = do
          saveURL (sub_file_url si) submitTXT
          return ()
            where
              submitDir
                | sub_status si == "SUCCEEDED"  =  "subs"
                | otherwise                     =  "subs" </> "err"
              submitTXT = submitDir </> idstr <.> "submit" <.> "txt"

---

checkCostsWithSubmissions :: String -> IO ()
checkCostsWithSubmissions engine = do
  let check1 si1 = do
        let submitTXT = "subs" </> show (sub1_id si1) <.> "submit" <.> "txt"
            checkFail m = putStrLn $ "  check failure:" ++ m
        putStrLn $ "checking cost of " ++ submitTXT
        either checkFail return =<< checkCostWithSubmission engine si1
  either fail (mapM_ check1 . filter ((== "SUCCEEDED") . sub1_status))
    =<< loadSubmissions "lists/submissions.json"

checkCostWithSubmission :: String -> Submission1 -> IO (Either String ())
checkCostWithSubmission engine si1 = do
  config <- getConfig
  src <- getSrcImage
  moves <- loadISL submitTXT
  target <- getTargetImage
  return $ do
    (result, calcCost) <- eval config src moves
    let sim = similarity result target
    when (calcCost + sim /= fromIntegral (sub1_score si1))
      $ Left $ "cost not match: " ++ show calcCost ++ " /= " ++ show (sub1_score si1)
    where
      eval
        | engine == "content"  =  Content.evalISLWithCost
        | otherwise            =  JuicyPixels.evalISLWithCost
      getConfig = do
        exist <- doesFileExist configJSON
        if exist
          then loadInitialConfig configJSON
          else return defaultInitialConfig
      configJSON = "probs" </> "ini" </> problemId <.> "initial" <.> "json"
      getSrcImage = do
        exist <- doesFileExist srcPNG
        if exist
          then Just <$> getImage srcPNG
          else return Nothing
      srcPNG = "probs" </> "ini" </> "src" </> problemId <.> "source" <.> "png"
      submitTXT = "subs" </> submissionId <.> "submit" <.> "txt"
      getTargetImage = getImage targetPNG
      targetPNG = "probs" </> problemId <.> "png"

      getImage fn = do
        do img' <- readImage fn
           case img' of
             Right (ImageRGBA8 img) ->  return img
             _                      ->  fail $ "not estimated image: " ++ fn
      submissionId = show $ sub1_id si1
      problemId = show $ sub1_problem_id si1

---

saveProblems :: IO ()
saveProblems = do
  let problemList = "lists/problems.json"
  executeCmd "./api/update-problems-list.sh" [problemList]
  saveProblems_ problemList

saveProblems_ :: FilePath -> IO ()
saveProblems_ = either fail (mapM_ saveProblem) <=< loadProblems

saveProblem :: Problem -> IO ()
saveProblem problem = do
  saveField prob_initial_config_link configJSON
  saveField prob_canvas_link canvasPNG
  saveField prob_target_link targetPNG
  saveSrc
  where
    name = show $ prob_id problem
    configJSON = "probs" </> "ini" </> name <.> "initial" <.> "json"
    canvasPNG = "probs" </> "ini" </> name <.> "initial" <.> "png"
    targetPNG = "probs" </> name <.> "png"
    saveField f out = saveURL (f problem) out
    srcPrefix = "probs" </> "ini" </> "src"
    saveSrcURL url = saveURL url $ srcPrefix </> takeFileName url
    saveSrc = do
      existConfig <- doesFileExist configJSON
      when existConfig $ do
        ic <- loadInitialConfig configJSON
        maybe (return ()) saveSrcURL $ icSourcePngJSON ic
        maybe (return ()) saveSrcURL $ icSourcePngPNG ic

---

saveURL :: FilePath -> FilePath -> IO ()
saveURL url output
  | "http" `isPrefixOf` url  =  executeCmd "./api/save-url.sh" [url, output]
  | otherwise                =  return ()  {- skipping -}

executeCmd :: String -> [String] -> IO ()
executeCmd cmd args = exitCode (return ()) (failWith cmdstr) =<< rawSystem cmd args
  where cmdstr = unwords $ cmd : args

failWith :: String -> Int -> IO a
failWith msg c = fail $ "Failure exit with code: " ++ show c ++ ": " ++ msg

exitCode :: a -> (Int -> a) -> ExitCode -> a
exitCode success failure ec = case ec of
  ExitSuccess    -> success
  ExitFailure c  -> failure c
