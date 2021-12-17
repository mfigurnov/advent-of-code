import Data.Char
import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Map as Map
import Control.Monad (guard, void)
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "38006F45291200"

-- Conversions

parseHexDigit :: Char -> Int
parseHexDigit c
  | '0' <= c && c <= '9' = ord c - ord '0'
  | 'A' <= c && c <= 'F' = 10 + ord c - ord 'A'
  | otherwise = undefined

decToBin :: Int -> String
decToBin c = reverse $ go c
  where go 0 = []
        go c = (if c `mod` 2 == 1 then '1' else '0') : go (c `div` 2)

padTo :: Int -> Char -> String -> String
padTo n c s = replicate (n - length s) c ++ s

hexDigitToBin :: Char -> String
hexDigitToBin = padTo 4 '0' . decToBin . parseHexDigit

binToDec :: String -> Int
binToDec = foldr (\elt carry -> (if elt == '0' then 0 else 1) + carry * 2) 0 . reverse

parseBinary :: Int -> ParsecT String () Identity Int
parseBinary n = binToDec <$> count n (oneOf "01")

-- Parser

data Packet = Packet Int Payload deriving (Show)
data Payload = Value Int | Operator PacketType [Packet] deriving (Show)
data PacketType = PSum | PProduct | PMinimum | PMaximum | PValue
  | PGreater | PLess | PEqual deriving (Eq, Show)

parsePacketType :: Int -> PacketType
parsePacketType 0 = PSum
parsePacketType 1 = PProduct
parsePacketType 2 = PMinimum
parsePacketType 3 = PMaximum
parsePacketType 4 = PValue
parsePacketType 5 = PGreater
parsePacketType 6 = PLess
parsePacketType 7 = PEqual

parsePacket :: ParsecT String () Identity Packet
parsePacket = try parseValuePacket <|> parseOperatorPacket

parseValuePacket :: ParsecT String () Identity Packet
parseValuePacket = do
  version <- parseBinary 3 <?> "could not parse value version"
  packetType <- parsePacketType <$> parseBinary 3
  guard (packetType == PValue) <?> "value: unexpected type id"
  value <- parseValue <?> "could not parse value"
  return (Packet version value)

parseValue :: ParsecT String () Identity Payload
parseValue = do
  first <- many (void (char '1') >> count 4 (oneOf "01"))
  final <- void (char '0') >> count 4 (oneOf "01")
  return (Value (binToDec $ concat first ++ final))

parseOperatorPacket :: ParsecT String () Identity Packet
parseOperatorPacket = do
  version <- parseBinary 3 <?> "could not parse operator version"
  packetType <- parsePacketType <$> parseBinary 3
  guard (packetType /= PValue) <?> "operator: unexpected type id"
  packets <- parseType0 <|> parseType1
  return (Packet version (Operator packetType packets))
  where parseType0 = do
          void $ char '0'
          numBits <- parseBinary 15
          payload <- count numBits (oneOf "01")
          return (parseWith (many parsePacket) payload)
        parseType1 = do
           void $ char '1'
           numPackets <- parseBinary 11
           count numPackets parsePacket

parseWith :: GenParser Char () a -> String -> a
parseWith parser body =
  case parse parser "[input]" body of
    Right x -> x
    Left e -> error (show e)

parseInput :: String -> Packet
parseInput = parseWith parsePacket . concatMap hexDigitToBin

-- Interpreter

sumPacketVersions :: Packet -> Int
sumPacketVersions (Packet v (Value _)) = v
sumPacketVersions (Packet v (Operator _ p)) = v + sum (map sumPacketVersions p)

solve = sumPacketVersions . parseInput

evalPacket :: Packet -> Int
evalPacket (Packet _ (Value x)) = x
evalPacket (Packet _ (Operator PSum p)) = sum (map evalPacket p)
evalPacket (Packet _ (Operator PProduct p)) = product (map evalPacket p)
evalPacket (Packet _ (Operator PMinimum p)) = minimum (map evalPacket p)
evalPacket (Packet _ (Operator PMaximum p)) = maximum (map evalPacket p)
evalPacket (Packet _ (Operator PGreater [p1, p2])) =
  if evalPacket p1 > evalPacket p2 then 1 else 0
evalPacket (Packet _ (Operator PLess [p1, p2])) =
  if evalPacket p1 < evalPacket p2 then 1 else 0
evalPacket (Packet _ (Operator PEqual [p1, p2])) =
  if evalPacket p1 == evalPacket p2 then 1 else 0

solvePart2 = evalPacket . parseInput

main = do
    file <- readFile "/tmp/day16-input.txt"
    print (solve file)
    print (solvePart2 file)
