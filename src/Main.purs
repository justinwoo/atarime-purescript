module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate, reverse, take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FRP.Event (Event, create, subscribe)
import Global.Unsafe (unsafeEncodeURIComponent)
import Milkis (URL(..), defaultFetchOptions, fetch, text)
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Simple.JSON (class ReadForeign, readJSON)
import TelegramBot (connect, onMessage, sendMessage)

type Config =
  { token :: String
  , master :: Int
  }

newtype Output = Output String
derive instance ntm :: Newtype Output _

newtype Query = Query String
derive instance ntq :: Newtype Query _

-- e.g. Data.Traversable.traverse
newtype QualifiedName = QualifiedName String
derive instance ntqn :: Newtype QualifiedName _
derive newtype instance rfqn :: ReadForeign QualifiedName

-- response from looking up on try purescript API
type LookupResponse =
  { results :: Array QualifiedName
  }

main :: Effect Unit
main = launchAff_ do
  c <- readJSON <$> readTextFile UTF8 "./config.json"
  case c of
    Right config -> do
      liftEffect $ runChocoPie main_ (drivers config)
    Left e -> do
      liftEffect $ log $ "Malformed config: " <> show e
  where
    main_ sources =
      { bot: sources.lookup
      , lookup: sources.bot
      }

    drivers config =
      { bot: bot config
      , lookup
      }

    getMessages connection = do
      {event, push} <- create
      onMessage connection $ \fM -> case runExcept fM of
        Right m
          | (Just text) <- m.text -> do
          push $ Query text
        _ -> do
          log "can't do shit cap'n"
      pure event

    bot :: Config -> Event Output -> Effect (Event Query)
    bot config outputs = do
      connection <- connect config.token
      _ <- subscribe outputs $ \(Output output) ->
        sendMessage connection config.master output
      getMessages connection

    baseUrl = "https://compile.purescript.org/try/search?q="

    lookup :: Event Query -> Effect (Event Output)
    lookup queries = do
      {event, push} <- create
      _ <- subscribe queries \(Query query) -> do
        launchAff_ do
          result <- text =<< fetch nodeFetch
            (URL $ baseUrl <> unsafeEncodeURIComponent query)
            defaultFetchOptions
          let
            output = case readJSON result of
              Right (lr :: LookupResponse) ->
                "Got results! You might try these: " <>
                  (intercalate "\n" $ unwrap <$> take 10 (reverse lr.results))
              Left e ->
                "Couldn't parse non-result JSON: " <> show result
          liftEffect <<< push <<< wrap $ output
      pure event
