{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (unless)

#if (defined(MIN_VERSION_http_directory) && MIN_VERSION_http_directory(0,1,5))
#else
import Network.HTTP.Client (Manager)
#endif
import Network.HTTP.Directory

import Control.Concurrent (threadDelay)

import Data.ByteUnits
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe

import SimpleCmd

import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeFileName, (</>))

main :: IO ()
main = do
  tasks <- do
    args <- getArgs
    if null args
      then do
      tasklines <- do
        mine <- kojiListBuildTasks Nothing
        if null mine then do
          mods <- kojiListBuildTasks $ Just "mbs/mbs.fedoraproject.org"
          if null mods
            then error' "no user or modular builds"
            else return mods
          else return mine
      return $ map (head . words) tasklines
      else return args
  btasks <- mapM kojiTaskinfoRecursive tasks
  mgr <- httpManager
  loopBuildTasks mgr btasks
  where
    kojiTaskinfoRecursive :: String -> IO BuildTask
    kojiTaskinfoRecursive tid = do
      output <- kojiTaskInfo tid
      let tasks = mapMaybe parseChunk $ chunks [] [] output
      return (tid, zip tasks (repeat Nothing))

    chunks :: [[String]] -> [String] -> [String] -> [[String]]
    chunks as [] [] = as
    chunks as chnk [] = as ++ [chnk]
    chunks as chnk ("":ls) = chunks (as ++ [chnk]) [] ls
    chunks as chnk (l:ls) =
      if ".rpm" `isSuffixOf` l && not (".src.rpm" `isSuffixOf` l)
      then chunks as chnk ls
      else chunks as (chnk ++ [l]) ls

type TaskId = String
type BuildTask = (TaskId, [TaskInfoSize])

type Size = Maybe Integer
type TaskInfoSize = (TaskInfo,Size)
type TaskInfoSizes = (TaskInfo,(Size,Size))

data TaskInfo =
  Taskinfo {taskId, taskNVR :: String,
            _taskArch :: String,
            taskState :: TaskState,
            _taskBuildLog :: String}
  deriving Show

type TaskState = String

-- second between polls
waitdelay :: Int
waitdelay = 120

loopBuildTasks :: Manager -> [BuildTask] -> IO ()
loopBuildTasks _ [] = return ()
loopBuildTasks mgr bts = do
  curs <- filter tasksOpen <$> mapM runProgress bts
  unless (null curs) $ do
    threadDelay (waitdelay * 1000000)
    news <- mapM updateBuildTask curs
    loopBuildTasks mgr news
  where
    runProgress :: BuildTask -> IO BuildTask
    runProgress (tid,tasks) = do
      unless (null tasks) $ do
        putStrLn ""
        logMsg $ taskNVR ((fst . head) tasks) ++ " (" ++ tid ++ ")"
      sizes <- mapM (buildlogSize mgr) tasks
      printLogSizes sizes
      let news = map (\(t,(s,_)) -> (t,s)) sizes
          open = filter (\ (t,_) -> taskState t == "open") news
      return (tid, open)

    tasksOpen :: BuildTask -> Bool
    tasksOpen (_,ts) = not (null ts)

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask (tid, ts) = do
      news <- mapM updateTask ts
      return (tid, news)

    updateTask :: TaskInfoSize -> IO TaskInfoSize
    updateTask (task,size) = do
      new <- parseChunk <$> kojiTaskInfo (taskId task)
      return (fromJust new,size)

parseChunk :: [String] -> Maybe TaskInfo
parseChunk ts =
  let task = mapMaybe (selectFields . splitOn ": ") ts in
    if lookup "Type" task /= Just "buildArch" then Nothing
    else do
      taskid <- lookup "Task" task
      nvr <- takeBaseName . takeBaseName <$> lookup "SRPM" task
      arch <- lookup "Build Arch" task
      state <- lookup "State" task
      buildlog <- lookup "Buildlog" task
      return $ Taskinfo taskid nvr arch state buildlog
  where
    selectFields :: [String] -> Maybe (String,String)
    selectFields [k,v] = Just (dropWhile (== ' ') k,v)
    selectFields [f] | "/build.log" `isSuffixOf` f = Just ("Buildlog", takeFileName f)
    selectFields _ = Nothing

buildlogSize :: Manager -> TaskInfoSize -> IO TaskInfoSizes
buildlogSize mgr (task@(Taskinfo tid _srpm _arch _state _), old) = do
  size <- httpFileSize mgr buildlog
  return (task,(size,old))
  where
    buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"
    lastFew =
      let few = dropWhile (== '0') $ drop 4 tid in
        if null few then "0" else few

data TaskOutput = TaskOut {_outArch :: String, outSize :: String, _outSpeed :: String, _outState :: String}

printLogSizes :: [TaskInfoSizes] -> IO ()
printLogSizes tss =
  mapM_ (putStrLn . taskOutList) (formatSize . map logSize tss)
  where
    taskOutList :: TaskOutput -> String
    taskOutList (TaskOut a si sp st) = unwords [a, si, sp, st]

    formatSize :: [TaskOutput] -> [TaskOutput]
    formatSize ts =
      let maxlen = maximum $ map (length . outSize) ts
      in map (justifySize maxlen) ts

    justifySize :: Int -> TaskOutput -> TaskOutput
    justifySize ml (TaskOut a si sp st) =
      TaskOut a (replicate (ml - length si) ' ' ++ si) sp st

    logSize :: TaskInfoSizes -> TaskOutput
    logSize (Taskinfo _tid _srpm arch state _, (size,old)) =
      let arch' = arch ++ padding
          size' = fmap humanSize size
          diff = (-) <$> size <*> old
          diff' = calcSpeed diff
          state' = if state == "open" then "" else " " ++ state
        in TaskOut arch' (fromMaybe "" size') (fromMaybe "" diff') state'
      where
        padding = replicate (8 - length arch) ' '

        calcSpeed :: Size -> Maybe String
        calcSpeed Nothing = Nothing
        calcSpeed (Just s) =
          Just $ " (" ++ humanSize (s `div` toInteger waitdelay) ++ "/s)"

        humanSize s =
          getShortHand $ getAppropriateUnits $ ByteValue (fromInteger s) Bytes

koji :: String -> [String] -> IO [String]
koji c args =
  cmdLines "koji" (c:args)

kojiTaskInfo :: String -> IO [String]
kojiTaskInfo tid =
  koji "taskinfo" ["-r", "-v", tid]

kojiListBuildTasks :: Maybe String -> IO [String]
kojiListBuildTasks muser = do
  res <- koji "list-tasks" $ ["--method=build", "--quiet"] ++ [maybe "--mine" ("--user=" ++) muser]
  return $ if res == ["(no tasks)"] then [] else res
