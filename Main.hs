import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (Status(..))

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: koji-progress <taskid>.."
    else do
    manager <- newManager tlsManagerSettings
    mapM_ (taskProgress (length args == 1) manager) args

taskProgress :: Bool -> Manager -> String -> IO ()
taskProgress loop manager task = do
  request <- parseRequest $ "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ task
  response <- httpLbs request manager
  processResponse response $ do
    let body = B.lines $ responseBody response
        -- <title>buildArch (ghc-8.4.4-72.fc28.src.rpm, armv7hl) | Task Info | koji</title>
        title = (B.words . head . filterByPrefix "    <title>") body
    if head title == B.pack "buildArch"
      then buildlogSize manager $ map B.pack [task, "class=taskopen", "", ""] ++ [B.init $ title !! 2]
      else do
      let tasks = map (B.words . B.filter (/= '\"')) . filterBySuffix ")</a>" . filterByPrefix "          <a href=\"taskinfo?taskID=" $ body
      showTasks tasks
      when loop $ loopTasks tasks
        where
          showTasks tasks = do
            printNVR tasks
            mapM_ (buildlogSize manager) tasks

          loopTasks tasks =
            threadDelay (60 * 10^6) >> showTasks tasks >> loopTasks tasks

type Task = [B.ByteString]

-- ["29967409","class=taskclosed","title=closed>buildArch","(ghc-8.4.3-71.module_1901+3deb4555.src.rpm,","x86_64"]
buildlogSize :: Manager -> Task -> IO ()
buildlogSize manager [taskid, state, _, _, arch] = do
  request <- parseRequest taskUrl
  response <- httpLbs request manager
  processResponse response $
    when (state == B.pack "class=taskopen") $ do
      B.putStr $ B.snoc arch ' '
      B.putStrLn $ last. B.words . head . filterByPrefix buildlogPrefix $ B.lines $ responseBody response
  where
    taskUrl = "https://kojipkgs.fedoraproject.org/work/tasks/" ++ lastFour ++ "/" ++ B.unpack taskid
    lastFour = drop 4 $ B.unpack taskid

    buildlogPrefix = "<img src=\"/icons/text.gif\" alt=\"[TXT]\"> <a href=\"build.log\">build.log</a>"

buildlogSize _ _ = return ()

processResponse :: Response a -> IO () -> IO ()
processResponse response action =
    case responseStatus response of
    Status 200 _ -> action
    Status n err -> BS.putStrLn err

filterByPrefix :: String -> [B.ByteString] -> [B.ByteString]
filterByPrefix cs = mapMaybe (B.stripPrefix (B.pack cs))

filterBySuffix :: String -> [B.ByteString] -> [B.ByteString]
filterBySuffix cs = mapMaybe (B.stripSuffix (B.pack cs))

printNVR :: [Task] -> IO ()
printNVR (t:ts) =
  if  length t == 5
  then B.putStrLn $ B.tail . B.init $ t !! 3 -- remove ( and ,
  else printNVR ts
printNVR _ = return ()
