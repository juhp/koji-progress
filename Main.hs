import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (Status(..))

import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  [task] <- getArgs
  manager <- newManager tlsManagerSettings

  request <- parseRequest $ "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ task
  response <- httpLbs request manager
  processResponse response $ do
    let tasks = map (B.words . B.filter (/= '\"')) . filterBySuffix ")</a>" . filterByPrefix "          <a href=\"taskinfo?taskID=" $ B.lines $ responseBody response
    mapM_ (buildlogSize manager) tasks

-- ["29967409","class=taskclosed","title=closed>buildArch","(ghc-8.4.3-71.module_1901+3deb4555.src.rpm,","x86_64"]
buildlogSize :: Manager -> [B.ByteString] -> IO ()
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
