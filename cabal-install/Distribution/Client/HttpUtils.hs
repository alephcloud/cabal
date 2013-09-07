-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists
-----------------------------------------------------------------------------
module Distribution.Client.HttpUtils (
    DownloadResult(..),
    downloadURI,
    getHTTP,
    cabalBrowse,
    proxy,
    isOldHackageURI
  ) where

import Network.HTTP ( Response (..) )
import Network.HTTP.Proxy ( Proxy(..), fetchProxy)
import Network.URI
         ( URI (..), URIAuth (..) )
import Network.Browser
         ( BrowserAction, browse
         , setOutHandler, setErrHandler, setProxy, setAuthorityGen )
import Network.Stream
         ( Result, ConnError(..) )
import Control.Monad
         ( liftM )
import Data.ByteString.Lazy (ByteString)

import qualified Paths_cabal_install (version)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils
         ( die, info, warn, debug, notice
         , copyFileVerbose, writeFileAtomic )
import Distribution.Text
         ( display )
import Data.Char ( isSpace )
import qualified System.FilePath.Posix as FilePath.Posix
         ( splitDirectories )
import System.FilePath
         ( (<.>) )
import System.Directory
         ( doesFileExist )

-- the remaining import are related to HTTPS support for getHTTP
import Control.Exception ( bracket )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO, liftIO )

import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI ( original, mk )
import Data.Default ( def )
import Data.Maybe ( mapMaybe )

import qualified Network.HTTP.Conduit as HTTPC
import Network.HTTP.Headers ( parseHeader, lookupHeader, HeaderName(HdrETag) )
import qualified Network.HTTP.Types.Header as HTTP ( hUserAgent )
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.TLS as TLS
         ( CertificateUsage( CertificateUsageAccept ) )

import System.IO ( hFlush, stdin, stdout, hGetEcho, hSetEcho )

data DownloadResult = FileAlreadyInCache | FileDownloaded FilePath deriving (Eq)

-- Trime
trim :: String -> String
trim = f . f
      where f = reverse . dropWhile isSpace

-- |Get the local proxy settings
--TODO: print info message when we're using a proxy based on verbosity
proxy :: Verbosity -> IO Proxy
proxy _verbosity = do
  p <- fetchProxy True
  -- Handle empty proxy strings
  return $ case p of
    Proxy uri auth ->
      let uri' = trim uri in
      if uri' == "" then NoProxy else Proxy uri' auth
    _ -> p

promptUsername :: MonadIO m => String -> m String
promptUsername realm = liftIO $ do
  putStr $ realm ++ " username: "
  hFlush stdout
  getLine

promptPassword :: MonadIO m => String -> m String
promptPassword realm = liftIO $ do
  putStr $ realm ++ " password: "
  hFlush stdout
  -- save/restore the terminal echoing status
  passwd <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
    hSetEcho stdin False  -- no echoing for entering the password
    getLine
  putStrLn ""
  return passwd

mkRequest :: URI
          -> Maybe String -- ^ Optional etag to check if we already have the latest file
          -> HTTPC.Request m
mkRequest uri etag = HTTPC.def

  { HTTPC.method = B8.pack "GET"
  , HTTPC.path = B8.pack $ uriPath uri
  , HTTPC.queryString = B8.pack $ uriQuery uri ++ uriFragment uri
  , HTTPC.host = B8.pack $ regName
  , HTTPC.port = port
  , HTTPC.secure = secure
  , HTTPC.requestHeaders = (HTTP.hUserAgent, B8.pack userAgent) : ifNoneMatchHdr
  , HTTPC.checkStatus = \_ _ _ -> Nothing
  }

  where
  ifNoneMatchHdr = maybe [] (\t -> [(CI.mk (B8.pack "if-none-match"), B8.pack t)]) etag
  userAgent = "cabal-install/" ++ display Paths_cabal_install.version

  (regName, portStr) = case uriAuthority uri of
      Nothing -> error "local URIs are not supported by this function" -- FIXME
      Just (URIAuth _ a b) -> (a, b)

  port = case portStr of
    "" -> if secure then 443 else 80
    p -> read $ drop 1 p

  secure = if uriScheme uri == "https:" then True else False

-- | FIXME
--
-- * respect proxy settings
--
-- * Set user agent Header
--
getHTTP :: Verbosity
        -> URI
        -> Maybe String -- ^ Optional etag to check if we already have the latest file.
        -> IO (Result (Response ByteString))
getHTTP _verbosity uri etag = do

  authInfo <- getAuthInfo

  withInsecureManager $ \manager -> do
    response <- case authInfo of
      Nothing -> do
        res <- HTTPC.httpLbs req manager
        if HTTPC.responseStatus res == HTTP.unauthorized401
          then do
            username <- liftM B8.pack $ promptUsername realm
            password <- liftM B8.pack $ promptPassword realm
            requestWithAuth username password manager
          else
            return $ responseToResponse res
      Just (username, password) ->
        requestWithAuth username password manager
    return $ Right response

  where

  req = mkRequest uri etag

  (userInfo, realm) = case uriAuthority uri of
    Nothing -> error "local URIs are not supported by this function" -- FIXME
    Just (URIAuth a b _) -> (a,b)

  -- FOR TESTING ONLY
  withInsecureManager = HTTPC.withManagerSettings settings
      where
      settings = def { HTTPC.managerCheckCerts = accept }
      accept _ _ _ = return TLS.CertificateUsageAccept

  requestWithAuth username password manager = do
        when (not (HTTPC.secure req)) . error $
          "Authentication is not supported over an insecure connection" -- FIXME
        let authReq = HTTPC.applyBasicAuth username password req
        liftM responseToResponse $ HTTPC.httpLbs authReq manager

  getAuthInfo = do
    case break (== ':') (dropEnd 1 userInfo) of
      ("", "")          -> return Nothing
      (user, "")        -> do
          p <- promptPassword realm
          return $ Just (B8.pack user, B8.pack p)
      (user, _colon : pwd) -> return $ Just (B8.pack user, B8.pack pwd)

  dropEnd i x = take (length x - i) x

responseToResponse :: HTTPC.Response ByteString -> Response ByteString
responseToResponse res = let status = HTTPC.responseStatus res in
  Response
    (responseCode status)
    (B8.unpack (HTTP.statusMessage status))
    (headersToHeaders (HTTPC.responseHeaders res))
    (HTTPC.responseBody res)

  where

  headersToHeaders headers = mapMaybe mapHeader headers
  mapHeader (hn, hv) = case parseHeader strHeader of
    Left _ -> Nothing
    Right h -> Just h
    where
    strHeader = B8.unpack (CI.original hn) ++ ":" ++ B8.unpack hv

  responseCode HTTP.Status{ HTTP.statusCode = statusInt } =
    case show statusInt of
      [a,b,c] -> (read [a], read [b], read [c])
      _ -> error $ "malformed HTTP status code: " ++ show statusInt -- FIXME

cabalBrowse :: Verbosity
            -> BrowserAction s ()
            -> BrowserAction s a
            -> IO a
cabalBrowse verbosity auth act = do
    p   <- proxy verbosity
    browse $ do
        setProxy p
        setErrHandler (warn verbosity . ("http error: "++))
        setOutHandler (debug verbosity)
        auth
        setAuthorityGen (\_ _ -> return Nothing)
        act

downloadURI :: Verbosity
            -> URI      -- ^ What to download
            -> FilePath -- ^ Where to put it
            -> IO DownloadResult
downloadURI verbosity uri path | uriScheme uri == "file:" = do
  copyFileVerbose verbosity (uriPath uri) path
  return (FileDownloaded path)
  -- Can we store the hash of the file so we can safely return path when the
  -- hash matches to avoid unnecessary computation?
downloadURI verbosity uri path = do
  let etagPath = path <.> "etag"
  targetExists   <- doesFileExist path
  etagPathExists <- doesFileExist etagPath
  -- In rare cases the target file doesn't exist, but the etag does.
  etag <- if targetExists && etagPathExists
            then liftM Just $ readFile etagPath
            else return Nothing

  result <- getHTTP verbosity uri etag
  let result' = case result of
        Left  err -> Left err
        Right rsp -> case rspCode rsp of
          (2,0,0) -> Right rsp
          (3,0,4) -> Right rsp
          (a,b,c) -> Left err
            where
              err = ErrorMisc $ "Error HTTP code: "
                                ++ concatMap show [a,b,c]

  -- Only write the etag if we get a 200 response code.
  -- A 304 still sends us an etag header.
  case result' of
    Left _ -> return ()
    Right rsp -> case rspCode rsp of
      (2,0,0) -> case lookupHeader HdrETag (rspHeaders rsp) of
        Nothing -> return ()
        Just newEtag -> writeFile etagPath newEtag
      (_,_,_) -> return ()

  case result' of
    Left err   -> die $ "Failed to download " ++ show uri ++ " : " ++ show err
    Right rsp -> case rspCode rsp of
      (2,0,0) -> do
        info verbosity ("Downloaded to " ++ path)
        writeFileAtomic path $ rspBody rsp
        return (FileDownloaded path)
      (3,0,4) -> do
        notice verbosity "Skipping download: Local and remote files match."
        return FileAlreadyInCache
      (_,_,_) -> return (FileDownloaded path)
      --FIXME: check the content-length header matches the body length.
      --TODO: stream the download into the file rather than buffering the whole
      --      thing in memory.

-- Utility function for legacy support.
isOldHackageURI :: URI -> Bool
isOldHackageURI uri
    = case uriAuthority uri of
        Just (URIAuth {uriRegName = "hackage.haskell.org"}) ->
            FilePath.Posix.splitDirectories (uriPath uri) == ["/","packages","archive"]
        _ -> False
