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
import Data.List
import Data.Maybe

import Fedora.Koji
import Fedora.Koji.Internal

import SimpleCmd
import SimpleCmdArgs

import System.FilePath (takeBaseName, (</>))

main :: IO ()
main =
  simpleCmdArgs' Nothing "koji-progress" "Watch Koji build.log sizes" $
    runOnTasks <$> many taskArg
  where
    taskArg = TaskId <$> argumentWith auto "TASKID"

runOnTasks :: [TaskID] -> IO ()
runOnTasks tids = do
  tasks <-
    if null tids
      then do
      mine <- kojiListBuildTasks Nothing
      if null mine then do
        mods <- kojiListBuildTasks $ Just "mbs/mbs.fedoraproject.org"
        if null mods
          then error' "no user or modular builds"
          else return mods
        else return mine
      else return tids
  btasks <- mapM kojiTaskinfoRecursive tasks
  mgr <- httpManager
  loopBuildTasks mgr btasks

kojiTaskinfoRecursive :: TaskID -> IO BuildTask
kojiTaskinfoRecursive tid = do
  children <- kojiGetTaskChildren fedoraKojiHub tid True
  return (tid, zip children (repeat Nothing))

type BuildTask = (TaskID, [TaskInfoSize])

type Size = Maybe Integer
-- FIXME change to (TaskID,Struct,Size)
type TaskInfoSize = (Struct,Size)
type TaskInfoSizes = (Struct,(Size,Size))

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
    runProgress (tid,tasks) =
      if null tasks then do
        state <- kojiGetTaskState fedoraKojiHub tid
        if state `elem` map Just openTaskStates then do
          threadDelay (waitdelay * 100000)
          kojiTaskinfoRecursive tid
          else return (tid,[])
      else do
        putStrLn ""
        let request = lookupStruct "request" $ fst (head tasks) :: Maybe [Value]
            nvr = case request of
                    Just params -> (takeBaseName . takeBaseName . maybeVal "failed to read src rpm" . getValue . head) params
                    Nothing -> error "No src rpm found"
        logMsg $ nvr ++ " (" ++ displayID tid ++ ")"
        sizes <- mapM (buildlogSize mgr) tasks
        printLogSizes sizes
        let news = map (\(t,(s,_)) -> (t,s)) sizes
            open = filter (\ (t,_) -> getTaskState t `elem` map Just openTaskStates) news
        return (tid, open)

    tasksOpen :: BuildTask -> Bool
    tasksOpen (_,ts) = not (null ts)

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask (tid, ts) = do
      news <- mapM updateTask ts
      return (tid, news)

    updateTask :: TaskInfoSize -> IO TaskInfoSize
    updateTask (task,size) = do
      let tid = fromJust (readID task)
      mnew <- kojiGetTaskInfo fedoraKojiHub tid
      case mnew of
        Nothing -> error' $ "TaskInfo not found for " ++ displayID tid
        Just new -> return (new,size)

buildlogSize :: Manager -> TaskInfoSize -> IO TaskInfoSizes
buildlogSize mgr (task, old) = do
  exists <- if isJust old then return True
            else httpExists mgr buildlog
  size <- if exists then httpFileSize mgr buildlog else return Nothing
  return (task,(size,old))
  where
    tid = show $ fromJust (readID' task)
    buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"
    lastFew =
      let few = dropWhile (== '0') $ drop 4 tid in
        if null few then "0" else few

data TaskOutput = TaskOut {_outArch :: String, outSize :: String, _outSpeed :: String, _outState :: String, _method :: String}

printLogSizes :: [TaskInfoSizes] -> IO ()
printLogSizes tss =
  mapM_ (putStrLn . taskOutList) $ (formatSize . map logSize) tss
  where
    taskOutList :: TaskOutput -> String
    taskOutList (TaskOut a si sp st mth) = unwords [a, si, mth, sp, st]

    formatSize :: [TaskOutput] -> [TaskOutput]
    formatSize ts =
      let maxlen = maximum $ map (length . outSize) ts
      in map (justifySize maxlen) ts

    justifySize :: Int -> TaskOutput -> TaskOutput
    justifySize ml (TaskOut a si sp st mth) =
      TaskOut a (replicate (ml - length si) ' ' ++ si) sp st mth

    logSize :: TaskInfoSizes -> TaskOutput
    logSize (task, (size,old)) =
      let method = maybeVal "method not found" $ lookupStruct "method" task :: String
          arch = maybeVal "arch not found" $ lookupStruct "arch" task :: String
          arch' = arch ++ replicate (8 - length arch) ' '
          size' = fmap humanSize size
          diff = (-) <$> size <*> old
          diff' = calcSpeed diff
          state = maybeVal "No state found" $ getTaskState task
          state' = if state == TaskOpen then "" else " " ++ show state
        in TaskOut arch' (fromMaybe "" size') (fromMaybe "" diff') state' method
      where
        calcSpeed :: Size -> Maybe String
        calcSpeed Nothing = Nothing
        calcSpeed (Just s) =
          Just $ " (" ++ humanSize (s `div` toInteger waitdelay) ++ "/s)"

        humanSize s =
          getShortHand $ getAppropriateUnits $ ByteValue (fromInteger s) Bytes

kojiListBuildTasks :: Maybe String -> IO [TaskID]
kojiListBuildTasks muser = do
  user <- case muser of
            Just user -> return user
            Nothing -> do
              mfasid <- (removeSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
              case mfasid of
                Just fas -> return fas
                Nothing -> error' "Could not determine FAS id from klist"
  mowner <- kojiGetUserID fedoraKojiHub user
  case mowner of
    Nothing -> error "No owner found"
    Just owner ->
      kojiListTaskIDs fedoraKojiHub [("method", ValueString "build"), ("owner", ValueInt (getID owner)), ("state", openTaskValues)] [("limit", ValueInt 10)]
