module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate, take)
import Data.Either (Either(..))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import FRP.Event (Event, create, subscribe)
import Global (encodeURIComponent)
import Milkis (defaultFetchOptions, fetch, text)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Simple.JSON (class ReadForeign, readJSON)
import TelegramBot (Message(Message), connect, onMessage, sendMessage)

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

yoloAff :: forall a e. Aff e a -> Eff e Unit
yoloAff aff =
  unit <$ runAff (const $ pure unit) (const $ pure unit) aff

main :: Eff _ _
main = launchAff do
  c <- readJSON <$> readTextFile UTF8 "./config.json"
  case runExcept c of
    Right config -> do
      liftEff $ runChocoPie main_ (drivers config)
    Left e -> do
      liftEff $ log $ "Malformed config: " <> show e
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
        Right (Message m)
          | NullOrUndefined (Just text) <- m.text -> do
          push $ Query text
        _ -> do
          log "can't do shit cap'n"
      pure event

    bot :: Config -> Event Output -> Eff _ (Event Query)
    bot config outputs = do
      connection <- connect config.token
      subscribe outputs $ \(Output output) ->
        sendMessage connection config.master output
      getMessages connection

    baseUrl = "https://compile.purescript.org/try/search?q="

    lookup :: Event Query -> Eff _ (Event Output)
    lookup queries = do
      {event, push} <- create
      subscribe queries \(Query query) -> do
        yoloAff do
          result <- text =<< fetch
            (baseUrl <> encodeURIComponent query)
            defaultFetchOptions
          let
            output = case runExcept $ readJSON result of
              Right (lr :: LookupResponse) ->
                "Got results! You might try these: " <>
                  (intercalate "\n" $ unwrap <$> take 10 lr.results)
              Left e ->
                "Couldn't parse non-result JSON: " <> show result
          liftEff <<< push <<< wrap $ output
      pure event