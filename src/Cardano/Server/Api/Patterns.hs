{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/patterns@ — the configured selectors, read-only.
module Cardano.Server.Api.Patterns
  ( PatternsAPI
  , patternsServer
  )
where

import Cardano.Server.Api.Common (badRequest, withReadConnection)
import Cardano.Sieve.Selector (includes, selectorFromText)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Database.SQLite.Simple (Only (Only), query_)

import Servant
  ( CaptureAll
  , Delete
  , Get
  , Handler
  , JSON
  , Put
  , Server
  , ServerError (errBody)
  , err501
  , throwError
  , (:<|>) ((:<|>))
  , (:>)
  )

-- | The configured selectors, read-only.
--
-- One 'CaptureAll' route per verb, because a pattern may span two path segments
-- (the @payment\/delegation@ form embeds a @\/@) and because it makes the bare
-- @\/patterns@ and @\/patterns\/{pattern}@ shapes one handler: no segments lists
-- everything, segments filter to the stored patterns that /include/ the given
-- one ('includes' — kupo's relation, so passing an address answers "which of my
-- selectors would match this?").
--
-- The write verbs exist to say no properly. kupo's PUT\/DELETE reconfigure a
-- RUNNING indexer — its handler rewinds the chain follower to re-index under
-- the new pattern set. Sieve's server deliberately has no indexer to rewind
-- (and when one shares the process, no channel to it), so these are 501 with
-- instructions, not 404: the resource exists, this server just will not mutate
-- it.
type PatternsAPI =
  "patterns" :> CaptureAll "pattern" Text :> Get '[JSON] [Text]
    :<|> "patterns" :> CaptureAll "pattern" Text :> Put '[JSON] Value
    :<|> "patterns" :> CaptureAll "pattern" Text :> Delete '[JSON] Value

-- | All three routes.
patternsServer :: FilePath -> Server PatternsAPI
patternsServer dbPath = patternsGet dbPath :<|> patternsRefuse :<|> patternsRefuse

-- | @GET \/patterns@ and @GET \/patterns\/{pattern}@ — the selectors this
-- database is indexed under, as stored (the canonical text 'reconcileSelectors'
-- wrote). No segments lists all of them; a pattern filters to those that
-- include it, and a malformed pattern is a 400.
patternsGet :: FilePath -> [Text] -> Handler [Text]
patternsGet dbPath segs = do
  stored <- liftIO $ withReadConnection dbPath $ \conn ->
    query_ conn "SELECT selector FROM patterns ORDER BY selector"
  let texts = [t | Only t <- stored]
  case segs of
    [] -> pure texts
    _ -> do
      needle <- case selectorFromText (T.intercalate "/" segs) of
        Left err -> badRequest ("invalid pattern: " <> pack (show err))
        Right sel -> pure sel
      -- Stored rows are canonical text written by selectorToText, so a parse
      -- failure here is corruption, not client error — let it 500 loudly.
      let parse t = either (\e -> error ("stored selector unparseable: " <> show e)) id (selectorFromText t)
      pure [t | t <- texts, parse t `includes` needle]

-- | @PUT@ and @DELETE@ under @\/patterns@: refused, with the reason and the
-- alternative in the body.
patternsRefuse :: [Text] -> Handler Value
patternsRefuse _ =
  throwError
    err501
      { errBody =
          "sieve does not reconfigure a live indexer: adding or removing \
          \patterns mid-sync leaves the database incomplete for what it claims \
          \to index (kupo re-syncs from a rollback point instead). Stop the \
          \indexer and restart it with the --select set you want; it will \
          \refuse mismatches and tell you what it was built with."
      }
