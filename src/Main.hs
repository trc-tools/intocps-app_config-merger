{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as DE
import Waargonaut.Types
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.Types       (Parser)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Control.Lens (over, _Left)
import Control.Applicative
import qualified Data.Aeson as A
import System.Directory

main :: IO ()
main = putStrLn "Hello"



readFile :: FilePath -> IO (Maybe BSL.ByteString)
readFile fp =
  doesFileExist fp >>=
  \fExists -> if fExists then Just <$> BSL.readFile fp else pure Nothing

decodeJson :: BSL.ByteString -> Maybe A.Object
decodeJson = A.decode

-- Waargonaut
--main :: IO ()
--main = let
--  json1 = "{\"a\":1}"
--  pjson1 = parseJson json1
--  json2 = "{\"a\":\"mjello\"}"
--  pjson2 = parseJson json2
--  in
--  do
--    printParseResult pjson1
--    printParseResult pjson2
--
--printParseResult :: Either (DE.DecodeError, D.CursorHistory) Json -> IO ()
--printParseResult pr =
--    case pr of
--      Left (x) -> Prelude.putStr . show $ x
--      Right (x) -> Prelude.putStr . show $ x
--
--parseJson :: ByteString -> Either (DE.DecodeError, D.CursorHistory) Json
--parseJson bs = D.simpleDecode D.json parseBS "{\"a\":1}"
--
---- The two functions below are taken from https://github.com/qfpl/waargonaut/blob/16eef35758dc918e97781ea69f173e6f45546b16/test/Types/Common.hs
--parseBS :: ByteString -> Either DE.DecodeError Json
--parseBS = parseWith AB.parseOnly parseWaargonaut
--
--parseWith :: (Parser t a -> t -> Either String a) -> Parser t a -> t -> Either DE.DecodeError a
--parseWith f p = over _Left (DE.ParseFailed . Text.pack . show) . f p
