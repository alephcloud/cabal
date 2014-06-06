-- This is a quick hack for uploading packages to Hackage.
-- See http://hackage.haskell.org/trac/hackage/wiki/CabalUpload

module Distribution.Client.Upload (check, upload, report) where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T

import Distribution.Client.Types (Username(..), Password(..),Repo(..),RemoteRepo(..))
import Distribution.Client.HttpUtils

import Distribution.Simple.Utils (debug, notice, warn, info)
import Distribution.Verbosity (Verbosity)
import Distribution.Text (display)
import Distribution.Client.Config

import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import qualified Distribution.Client.BuildReports.Upload as BuildReport

import Network.Browser
         ( Authority(..), addAuthority )
import Network.HTTP
         ( HeaderName(..), findHeader, Response(..) )
import Network.URI (URI(uriPath), parseURI)

import Data.Char        (intToDigit)
import System.IO        (hFlush, stdin, stdout, hGetEcho, hSetEcho)
import Control.Exception (bracket)
import System.FilePath  ((</>), takeExtension)
import qualified System.FilePath.Posix as FilePath.Posix (combine)
import System.Directory
import Control.Monad (forM_, when)

import qualified Network.HTTP.Conduit as HTTPC
import qualified Network.HTTP.Client.MultipartFormData as HTTPC

--FIXME: how do we find this path for an arbitrary hackage server?
-- is it always at some fixed location relative to the server root?
legacyUploadURI :: URI
Just legacyUploadURI = parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/protected/upload-pkg"

checkURI :: URI
Just checkURI = parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/check-pkg"

upload :: Verbosity -> [Repo] -> Maybe Username -> Maybe Password -> [FilePath] -> IO ()
upload verbosity repos _mUsername _mPassword paths = do
          let uploadURI = if isOldHackageURI targetRepoURI
                          then legacyUploadURI
                          else targetRepoURI{uriPath = uriPath targetRepoURI `FilePath.Posix.combine` "upload"}
          flip mapM_ paths $ \path -> do
            notice verbosity $ "Uploading " ++ path ++ "... "
            handlePackage verbosity uploadURI path
  where
    targetRepoURI = remoteRepoURI $ last [ remoteRepo | Left remoteRepo <- map repoKind repos ] --FIXME: better error message when no repos are given

promptUsername :: IO Username
promptUsername = do
  putStr "Hackage username: "
  hFlush stdout
  fmap Username getLine

promptPassword :: IO Password
promptPassword = do
  putStr "Hackage password: "
  hFlush stdout
  -- save/restore the terminal echoing status
  passwd <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
    hSetEcho stdin False  -- no echoing for entering the password
    fmap Password getLine
  putStrLn ""
  return passwd

report :: Verbosity -> [Repo] -> Maybe Username -> Maybe Password -> IO ()
report verbosity repos mUsername mPassword = do
      let uploadURI = if isOldHackageURI targetRepoURI
                      then legacyUploadURI
                      else targetRepoURI{uriPath = ""}
      Username username <- maybe promptUsername return mUsername
      Password password <- maybe promptPassword return mPassword
      let auth = addAuthority AuthBasic {
                   auRealm    = "Hackage",
                   auUsername = username,
                   auPassword = password,
                   auSite     = uploadURI
                 }

      -- FIXME: I think there should be some way to configure which repositories are used
      -- for upload.
      let remoteRepo = last [ r | Left r <- map repoKind repos ]
      dotCabal <- defaultCabalDir
      let srcDir = dotCabal </> "reports" </> remoteRepoName remoteRepo
      -- We don't want to bomb out just because we haven't built any packages from this repo yet
      srcExists <- doesDirectoryExist srcDir
      when srcExists $ do
        contents <- getDirectoryContents srcDir
        forM_ (filter (\c -> takeExtension c == ".log") contents) $ \logFile ->
            do inp <- readFile (srcDir </> logFile)
               let (reportStr, buildLog) = read inp :: (String,String)
               case BuildReport.parse reportStr of
                 Left errs -> do warn verbosity $ "Errors: " ++ errs -- FIXME
                 Right report' ->
                     do info verbosity $ "Uploading report for " ++ display (BuildReport.package report')
                        cabalBrowse verbosity auth $ BuildReport.uploadReports (remoteRepoURI remoteRepo) [(report', Just buildLog)]
                        return ()

  where
    targetRepoURI = remoteRepoURI $ last [ remoteRepo | Left remoteRepo <- map repoKind repos ] --FIXME: better error message when no repos are given

check :: Verbosity -> [FilePath] -> IO ()
check verbosity paths = do
          flip mapM_ paths $ \path -> do
            notice verbosity $ "Checking " ++ path ++ "... "
            handlePackage verbosity checkURI path

handlePackage :: Verbosity -> URI -> FilePath -> IO ()
handlePackage verbosity uri path =
  do req <- mkUploadRequest uri path
     debug verbosity $ "\n" ++ show req
     resp <- doHTTP verbosity uri req >>= \x -> case x of -- cabalBrowse verbosity auth $ request req
         Left e -> error $ "Connection failure: " ++ show e
         Right r -> return r
     debug verbosity $ show resp
     case rspCode resp of
       (2,0,0) -> do notice verbosity "Ok"
       (x,y,z) -> do notice verbosity $ "Error: " ++ path ++ ": "
                                     ++ map intToDigit [x,y,z] ++ " "
                                     ++ rspReason resp
                     case findHeader HdrContentType resp of
                       Just contenttype
                         | takeWhile (/= ';') contenttype == "text/plain"
                         -> notice verbosity $ B8.unpack (rspBody resp)
                       _ -> debug verbosity $ B8.unpack (rspBody resp)

-- use sequece: mkRequest and doHTTP

mkUploadRequest :: URI -> FilePath -> IO HTTPC.Request
mkUploadRequest uri path =
    HTTPC.formDataBody
        [ HTTPC.partFileSource (T.pack "package") path ]
        (mkRequest "POST" uri Nothing Nothing)

