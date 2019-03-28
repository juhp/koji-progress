{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (Status(..))
import Network.URI

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (getArgs)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: koji-progress <taskid|taskurl>.."
    else do
    manager <- newManager tlsManagerSettings
    mapM_ (taskProgress (length args == 1) manager) args

taskProgress :: Bool -> Manager -> String -> IO ()
taskProgress loop manager task = do
  let url = if isURI task then task else "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ task
  request <- parseRequest url
  response <- httpLbs request manager
  processResponse response $ do
    let cursor = fromDocument $ parseLBS $ responseBody response

        -- <title>buildArch (ghc-8.4.4-72.fc28.src.rpm, armv7hl) | Task Info | koji</title>
    let title = T.words . T.concat $ cursor $/ element "head" &/ element "title" &// content
    if head title == "buildArch"
      then buildlogSize manager False $ Task task "" (T.init . T.tail $ title !! 1) (T.init $ title !! 2)
      else do
      let tasks = mapMaybe linkToTask $ cursor $/ element "body" &// element "span" >=> attributeIs "class" "treeLabel" &/ element "a"
      showTasks True tasks
      when loop $ loopTasks tasks
        where
          showTasks closed tasks = do
            T.putStrLn . nvr $ head tasks
            mapM_ (buildlogSize manager closed) tasks

          loopTasks tasks =
            when (any open tasks) $
            threadDelay (60 * 1000000) >> showTasks False tasks >> loopTasks tasks
            where
              open :: Task -> Bool
              open t = state t == "open"

data Task = Task {_taskid :: String, state :: T.Text, nvr :: T.Text, _arch :: T.Text}

linkToTask :: Cursor -> Maybe Task
linkToTask e =
  let cnt = content . head $ child e
      txt = T.words . head $ cnt in
    if head txt == "buildArch"
    then
      let tstate = head $ attribute "title" e
          href = attribute "href" e
          tid = T.unpack $ fromMaybe (error "bad href") $ T.stripPrefix "taskinfo?taskID=" $ head href
          tnvr = T.init . T.tail $ txt !! 1
          tarch = T.init $ txt !! 2
      in Just $ Task tid tstate tnvr tarch
    else Nothing

buildlogSize :: Manager -> Bool -> Task -> IO ()
buildlogSize manager closed (Task taskid tstate _nvr arch) = do
  request <- parseRequest taskUrl
  response <- httpLbs request manager
  let open = tstate == "open"
  processResponse response $
    when (closed || open) $ do
      T.putStr $ T.append arch " "
      let cursor = fromDocument $ parseLBS $ responseBody response
          buildlog = T.words . head . content . head $ cursor $/ element "body" &// element "a" >=> attributeIs "href" "build.log" >=> followingSibling
      T.putStr $ buildlog !! 2
      T.putStrLn $ if closed then (if open then "" else T.cons ' ' tstate) else ""
        where
          taskUrl = "https://kojipkgs.fedoraproject.org/work/tasks/" ++ lastFew ++ "/" ++ taskid
          lastFew =
            let few = dropWhile (== '0') $ drop 4 taskid in
              if null few then "0" else few

processResponse :: Response a -> IO () -> IO ()
processResponse response action =
    case responseStatus response of
    Status 200 _ -> action
    Status n err -> B.putStrLn $ B.append (B.pack $ show n ++ " ") err
