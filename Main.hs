{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client (Manager)
import Network.HTTP.Directory

import Control.Concurrent (threadDelay)

import Data.ByteUnits
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)

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
        if mine == ["(no tasks)"] then
          kojiListBuildTasks $ Just "mbs/mbs.fedoraproject.org"
          else return mine
      return $ map (head . words) tasklines
      else return args
  getBuildTask tasks

type BuildTask = [TaskInfo]

getBuildTask :: [String] -> IO ()
getBuildTask tasks = do
  btasks <- mapM kojiTaskinfoRecursive tasks
  mgr <- httpManager
  checkBuildTasks mgr btasks
  where
    kojiTaskinfoRecursive :: String -> IO [TaskInfo]
    kojiTaskinfoRecursive tid = do
      output <- kojiTaskInfo tid
      return $ mapMaybe parseChunk $ chunks [] [] output

    chunks :: [[String]] -> [String] -> [String] -> [[String]]
    chunks as [] [] = as
    chunks as chnk [] = as ++ [chnk]
    chunks as chnk ("":ls) = chunks (as ++ [chnk]) [] ls
    chunks as chnk (l:ls) =
      if ".rpm" `isSuffixOf` l && not (".src.rpm" `isSuffixOf` l)
      then chunks as chnk ls
      else chunks as (chnk ++ [l]) ls

checkBuildTasks :: Manager -> [BuildTask] -> IO ()
checkBuildTasks _ [] = return ()
checkBuildTasks mgr bts = do
  cur <- mapM runProgress bts
  threadDelay (60 * 1000000)
  new <- mapM updateBuildTask cur
  checkBuildTasks mgr new
  where
    runProgress :: BuildTask -> IO BuildTask
    runProgress tasks = do
      putStrLn $ taskNVR (head tasks)
      mapM_ (buildlogSize mgr) tasks
      return $ filter (\ t -> taskState t == "open") tasks

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask tasks =
      catMaybes <$> mapM updateTask tasks

    updateTask :: TaskInfo -> IO (Maybe TaskInfo)
    updateTask task =
      parseChunk <$> kojiTaskInfo (taskId task)

data TaskInfo =
  Taskinfo {taskId, taskNVR :: String,
            _taskArch :: String,
            taskState :: TaskState,
            _taskBuildLog :: String}

type TaskState = String

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

buildlogSize :: Manager -> TaskInfo -> IO ()
buildlogSize mgr (Taskinfo tid _srpm arch state _) = do
  putStr $ arch ++ " "
  size <- httpFileSize mgr buildlog
  let humanSize s =
        getShortHand $ getAppropriateUnits $ ByteValue (fromInteger s) Bytes
  maybe (return ()) (putStr . humanSize) size
  putStrLn $ if state == "open" then "" else " " ++ state
      where
        buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"
        lastFew =
          let few = dropWhile (== '0') $ drop 4 tid in
            if null few then "0" else few

koji :: String -> [String] -> IO [String]
koji c args =
  cmdLines "koji" (c:args)

kojiTaskInfo :: String -> IO [String]
kojiTaskInfo tid =
  koji "taskinfo" ["-r", "-v", tid]

kojiListBuildTasks :: Maybe String -> IO [String]
kojiListBuildTasks muser =
  koji "list-tasks" $ ["--method=build", "--quiet"] ++ [maybe "--mine" ("--user=" ++) muser]
