{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (unless)

import Network.HTTP.Client (Manager)
import Network.HTTP.Directory

import Control.Concurrent (threadDelay)

import Data.ByteUnits
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)

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
    kojiTaskinfoRecursive :: String -> IO [TaskInfoSize]
    kojiTaskinfoRecursive tid = do
      output <- kojiTaskInfo tid
      let tasks = mapMaybe parseChunk $ chunks [] [] output
      return $ zip tasks (repeat Nothing)

    chunks :: [[String]] -> [String] -> [String] -> [[String]]
    chunks as [] [] = as
    chunks as chnk [] = as ++ [chnk]
    chunks as chnk ("":ls) = chunks (as ++ [chnk]) [] ls
    chunks as chnk (l:ls) =
      if ".rpm" `isSuffixOf` l && not (".src.rpm" `isSuffixOf` l)
      then chunks as chnk ls
      else chunks as (chnk ++ [l]) ls

type BuildTask = [TaskInfoSize]

type Size = Maybe Integer
type TaskInfoSize = (TaskInfo,Size)
type TaskInfoSizes = (TaskInfo,(Size,Size))

data TaskInfo =
  Taskinfo {taskId, taskNVR :: String,
            _taskArch :: String,
            taskState :: TaskState,
            _taskBuildLog :: String}

type TaskState = String

loopBuildTasks :: Manager -> [BuildTask] -> IO ()
loopBuildTasks _ [] = return ()
loopBuildTasks mgr bts = do
  curs <- filter (not . null) <$> mapM runProgress bts
  unless (null curs) $ do
    threadDelay (60 * 1000000)
    news <- filter (not . null) <$> mapM updateBuildTask curs
    loopBuildTasks mgr news
  where
    runProgress :: BuildTask -> IO BuildTask
    runProgress tasks = do
      unless (null tasks) $ do
        putStrLn ""
        putStrLn $ taskNVR $ (fst . head) tasks
      sizes <- mapM (buildlogSize mgr) tasks
      mapM_ printLogSize sizes
      let news = map (\(t,(s,_)) -> (t,s)) sizes
      return $ filter (\ (t,_) -> taskState t == "open") news

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask = mapM updateTask

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

printLogSize :: TaskInfoSizes -> IO ()
printLogSize (Taskinfo _tid _srpm arch state _, (size,old)) = do
  putStr $ arch ++ " "
  maybe (return ()) (putStr . humanSize) size
  let diff = (-) <$> size <*> old
  putSpeed diff
  putStrLn $ if state == "open" then "" else " " ++ state
  where
    putSpeed :: Size -> IO ()
    putSpeed Nothing = return ()
    putSpeed (Just s) = do
      putStr " ("
      putStr $ humanSize s
      putStr "/min)"

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
