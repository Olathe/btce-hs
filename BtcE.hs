{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings, BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

-- BTC-E.com API

module Network.BtcE where

import qualified Data.ListLike              as LL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8      as SB
import qualified Data.Vector                as V
import qualified Data.Text                  as T
import qualified Data.HashMap.Strict        as H
import qualified Data.Graph.Inductive.Graph as G

import Data.Bits                   (xor)
import Data.Word                   (Word8, Word16, Word32)
import Data.Char                   (ord, chr, toLower, isDigit)
import Data.List                   (foldl', stripPrefix, group, sort)
import Data.Graph.Inductive.Graph  (mkGraph, ufold)
import Data.Graph.Inductive.Tree   (Gr)

import System.Time                (ClockTime(TOD))
import Data.Digest.Pure.SHA       (hmacSha512, showDigest)
import Control.Applicative        ((<$>), (<*>), pure, liftA2)
import Control.Monad              (when, join, liftM, liftM2)
import Control.Monad.Trans.Class  (lift)
import Control.Arrow              ((***), (>>^), first, second)

import Control.Concurrent.MVar      (MVar, newMVar, takeMVar, putMVar)
import Control.Monad.Trans.Resource (ResourceT)
import Network.HTTP.Base            (urlEncodeVars)
import Network.HTTP.Conduit         (Manager, newManager, httpLbs,
                                     Request, RequestBody(RequestBodyLBS), requestHeaders, requestBody, urlEncodedBody, responseBody,
                                     parseUrl, secure, host, port, path, responseTimeout, decompress, alwaysDecompress)
import Data.Aeson                   (FromJSON, Value(Object, Array, String, Number), (.:), parseJSON, eitherDecode)
import Data.Aeson.Types             (Parser)

import Data.Typeable      (Typeable)
import Data.Hashable      (Hashable(..))
import Control.Exception  (Exception, throw)

-- ==== REQUESTS ==== --

trades :: Word16 -> String -> Manager -> ResourceT IO (Either String (H.HashMap String [CompletedTradeInfo]))
trades count currencyPair =  makeRequest (SB.append "trades/" . SB.pack $ currencyPair) [("limit", show count)]

info :: Manager -> ResourceT IO (Either String (H.HashMap (Currency, Currency) PairInfo))
info = liftM (rightMap unwrapExchangeInfo) . makeRequest "info" []

infoGraph :: Manager -> ResourceT IO (Either String (Gr Currency PairInfo))
infoGraph = liftM (rightMap convertInfo) . info

ticker :: String -> Manager -> ResourceT IO (Either String (H.HashMap String Ticker))
ticker currencyPair = makeRequest (SB.append "ticker/" . SB.pack $ currencyPair) []

offers :: Word32 -> String -> Manager -> ResourceT IO (Either String (H.HashMap String Offers))
offers count currencyPair =  makeRequest (SB.append "depth/" . SB.pack $ currencyPair) [("limit", show count)]

myInfo :: Account -> Manager -> ResourceT IO (Either String MyInfo)
myInfo = makeSecureRequest "getInfo" []

makeTrade :: String -> TradeType -> Rational -> Rational -> Account -> Manager -> ResourceT IO (Either String PlacedTrade)
makeTrade !pair !tradeType !rate !quantity = makeSecureRequest "Trade" [("pair", pair), ("type", if tradeType == Bid then "buy" else "sell"), ("rate", showRational rate), ("amount", showRational quantity)]

-- ==== DATA TYPES ==== --

class Timestamped a where
  timestamp :: a -> ClockTime

class FundsListing a where
  funds :: a -> H.HashMap Currency Rational

-- == Currency and Amount== --

type Currency = Rational -> Amount

data Amount = BTC Rational
            | LTC Rational
            | FTC Rational
            | NMC Rational
            | NVC Rational
            | PPC Rational
            | TRC Rational
            | USD Rational
            | EUR Rational
            | RUR Rational
            | NeedsToBeAdded !String Rational
  deriving (Show, Eq, Ord)

instance Hashable Currency where
  hashWithSalt n x = xor (16777619*n) (fromEnum x)

instance Show Currency where
  showsPrec p x = showParen (p > 0) . showString $
                    case x undefined of
                      BTC _ -> "BTC"
                      LTC _ -> "LTC"
                      FTC _ -> "FTC"
                      NMC _ -> "NMC"
                      NVC _ -> "NVC"
                      PPC _ -> "PPC"
                      TRC _ -> "TRC"
                      USD _ -> "USD"
                      EUR _ -> "EUR"
                      RUR _ -> "RUR"
                      NeedsToBeAdded xs _ -> "NeedsToBeAdded " ++ xs

instance Eq Currency where
  x == y = case (x undefined, y undefined) of
             (BTC _, BTC _) -> True
             (LTC _, LTC _) -> True
             (FTC _, FTC _) -> True
             (NMC _, NMC _) -> True
             (NVC _, NVC _) -> True
             (PPC _, PPC _) -> True
             (TRC _, TRC _) -> True
             (USD _, USD _) -> True
             (EUR _, EUR _) -> True
             (RUR _, RUR _) -> True
             (NeedsToBeAdded xs _, NeedsToBeAdded ys _) -> xs == ys
             _              -> False

instance Ord Currency where
  compare x y = if   x == y then EQ
                else case (x undefined, y undefined) of
                       (BTC _, _) -> LT
                       (_, BTC _) -> GT
                       (LTC _, _) -> LT
                       (_, LTC _) -> GT
                       (FTC _, _) -> LT
                       (_, FTC _) -> GT
                       (NMC _, _) -> LT
                       (_, NMC _) -> GT
                       (NVC _, _) -> LT
                       (_, NVC _) -> GT
                       (PPC _, _) -> LT
                       (_, PPC _) -> GT
                       (TRC _, _) -> LT
                       (_, TRC _) -> GT
                       (USD _, _) -> LT
                       (_, USD _) -> GT
                       (EUR _, _) -> LT
                       (_, EUR _) -> GT
                       (RUR _, _) -> LT
                       (_, RUR _) -> GT
                       (NeedsToBeAdded xs _, NeedsToBeAdded ys _) -> compare xs ys

instance Enum Currency where
  fromEnum x = case x undefined of
                 BTC _ -> 0
                 LTC _ -> 1
                 FTC _ -> 2
                 NMC _ -> 3
                 NVC _ -> 4
                 PPC _ -> 5
                 TRC _ -> 6
                 USD _ -> 7
                 EUR _ -> 8
                 RUR _ -> 9
                 NeedsToBeAdded xs _ -> join seq . fromInteger . uncurry (+) . first head . foldl' (\((_:bs), !r) e -> (bs, 26*r + e)) (letterCountBreaks, 0) . map (toInteger . subtract (ord 'a') . ord) $ xs
  
  toEnum 0 = BTC
  toEnum 1 = LTC
  toEnum 2 = FTC
  toEnum 3 = NMC
  toEnum 4 = NVC
  toEnum 5 = PPC
  toEnum 6 = TRC
  toEnum 7 = USD
  toEnum 8 = EUR
  toEnum 9 = RUR
  toEnum x = let !x' = toInteger x
                 (!len, !bnd) = desize x'
             in  join seq . NeedsToBeAdded . join seq . map (join seq . chr . (+ (ord 'a')) . fromInteger . snd) . reverse . take len . tail . iterate (flip divMod 26 . fst) . flip (,) 0 . subtract bnd $ x'
    where
      desize :: Integer -> (Int, Integer)
      desize n = let !n' = (flip mod intUpperBound) . toInteger $ n
                 in second fromInteger . last . filter ((<= n') . snd) $ boundedLetterCountBreaks

toCurrency :: String -> Currency
toCurrency "btc" = BTC
toCurrency "ltc" = LTC
toCurrency "ftc" = FTC
toCurrency "nmc" = NMC
toCurrency "nvc" = NVC
toCurrency "ppc" = PPC
toCurrency "trc" = TRC
toCurrency "usd" = USD
toCurrency "eur" = EUR
toCurrency "rur" = RUR
toCurrency x     = NeedsToBeAdded x

fromCurrency :: Currency -> String
fromCurrency x = case x undefined of
                   BTC _ -> "btc"
                   LTC _ -> "ltc"
                   FTC _ -> "ftc"
                   NMC _ -> "nmc"
                   NVC _ -> "nvc"
                   PPC _ -> "ppc"
                   TRC _ -> "trc"
                   USD _ -> "usd"
                   EUR _ -> "eur"
                   RUR _ -> "rur"
                   NeedsToBeAdded x _ -> x

-- == TradeType == --

data TradeType = Bid | Ask
  deriving (Show, Eq, Ord)

-- == Account == -- 

data Account = Account {-# UNPACK #-} !SB.ByteString
                                      !LB.ByteString
                       {-# UNPACK #-} !(MVar Word32)

instance Show Account where
  showsPrec p (Account key secret _) = showParen (p > 0) $ showString "getAccount " . shows key . showChar ' ' . shows secret

instance Eq Account where
  (Account key1 secret1 _) == (Account key2 secret2 _) = (key1 == key2) && (secret1 == secret2)

getAccount :: (LL.StringLike a, LL.StringLike b) => a -> b -> ResourceT IO Account
getAccount key secret = lift (newMVar 1) >>= return . Account (SB.pack (LL.toString key)) (LB.pack (LL.toString secret))

data BtcEException = APIKeyUsedUp
                   | ServerError !String
  deriving (Show, Typeable)

instance Exception BtcEException

-- == CompletedTradeInfo == --

data CompletedTradeInfo = CompletedTradeInfo {-# UNPACK #-} !ClockTime
                                                            !Integer
                                                            !TradeType
                                             {-# UNPACK #-} !Offer

instance Timestamped CompletedTradeInfo where
  {-# SPECIALIZE instance Timestamped CompletedTradeInfo #-}
  timestamp   (CompletedTradeInfo x _ _ _) = x
transactionID (CompletedTradeInfo _ x _ _) = x
tradeType     (CompletedTradeInfo _ _ x _) = x
offer         (CompletedTradeInfo _ _ _ x) = x

instance Show CompletedTradeInfo where
  showsPrec p x = showParen (p > 0)
                $ showString "CompletedTradeInfo {"
                . showString "timestamp = "       . showsPrec 0 (timestamp     x)
                . showString ", transactionID = " . showsPrec 0 (transactionID x)
                . showString ", tradeType = "     . showsPrec 0 (tradeType     x)
                . showString ", offer = "         . showsPrec 0 (offer         x)
                . showChar   '}'

instance FromJSON CompletedTradeInfo where
  parseJSON (Object !v) = CompletedTradeInfo <$> liftM (flip TOD 0)                                           (v .: "timestamp")
                                             <*>                                                              (v .: "tid")
                                             <*> liftM  (\x   -> if x == ("bid" :: String) then Bid else Ask) (v .: "type")
                                             <*> liftM2 Offer                                                 (v .: "price")     (v .: "amount")
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

-- == PairInfo == --

data PairInfo = PairInfo {-# UNPACK #-} !ClockTime
                                        !Currency
                                        !Currency
                         {-# UNPACK #-} !Rational
                         {-# UNPACK #-} !Rational
                         {-# UNPACK #-} !Rational
                         {-# UNPACK #-} !Rational
                                        !String
                                        !Bool
                         {-# UNPACK #-} !Word8
                                        !Bool

instance Timestamped PairInfo where
  {-# SPECIALIZE instance Timestamped PairInfo #-}
  timestamp     (PairInfo x _ _ _ _ _ _ _ _ _ _) = x
from            (PairInfo _ x _ _ _ _ _ _ _ _ _) = x
to              (PairInfo _ _ x _ _ _ _ _ _ _ _) = x
minimumPrice    (PairInfo _ _ _ x _ _ _ _ _ _ _) = x
maximumPrice    (PairInfo _ _ _ _ x _ _ _ _ _ _) = x
minimumQuantity (PairInfo _ _ _ _ _ x _ _ _ _ _) = x
fee             (PairInfo _ _ _ _ _ _ x _ _ _ _) = x
rawPair         (PairInfo _ _ _ _ _ _ _ x _ _ _) = x
reversed        (PairInfo _ _ _ _ _ _ _ _ x _ _) = x
decimalPlaces   (PairInfo _ _ _ _ _ _ _ _ _ x _) = x
hidden          (PairInfo _ _ _ _ _ _ _ _ _ _ x) = x

instance Show PairInfo where
  showsPrec p x = showParen (p > 0)
                $ showString "PairInfo {"
                . showString "timestamp = "         . showsPrec 0 (timestamp       x)
                . showString ", from = "            . showsPrec 0 (from            x)
                . showString ", to = "              . showsPrec 0 (to              x)
                . showString ", minimumPrice = "    . showsPrec 0 (minimumPrice    x)
                . showString ", maximumPrice = "    . showsPrec 0 (maximumPrice    x)
                . showString ", minimumQuantity = " . showsPrec 0 (minimumQuantity x)
                . showString ", fee = "             . showsPrec 0 (fee             x)
                . showString ", rawPair = "         . showsPrec 0 (rawPair         x)
                . showString ", reversed = "        . showsPrec 0 (reversed        x)
                . showString ", decimalPlaces = "   . showsPrec 0 (decimalPlaces   x)
                . showString ", hidden = "          . showsPrec 0 (hidden          x)
                . showChar   '}'

data PairInfo' = PairInfo' {-# UNPACK #-} !Rational
                           {-# UNPACK #-} !Rational
                           {-# UNPACK #-} !Rational
                           {-# UNPACK #-} !Rational
                           {-# UNPACK #-} !Word8
                                          !Bool

finishPairInfo :: ClockTime -> Currency -> Currency -> String -> Bool -> PairInfo' -> PairInfo
finishPairInfo timestamp from to rawPair reversed (PairInfo' a b c d e f) = PairInfo timestamp from to a b c d rawPair reversed e f

instance FromJSON PairInfo' where
  parseJSON (Object !v) = PairInfo' <$> v .: "min_price"
                                    <*> v .: "max_price"
                                    <*> v .: "min_amount"
                                    <*> liftM (/100) (v .: "fee")
                                    <*> v .: "decimal_places"
                                    <*> liftM (/= (0 :: Word8)) (v .: "hidden")
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

newtype ExchangeInfo = ExchangeInfo (H.HashMap (Currency, Currency) PairInfo)

unwrapExchangeInfo :: ExchangeInfo -> H.HashMap (Currency, Currency) PairInfo
unwrapExchangeInfo (ExchangeInfo !x) = x

instance FromJSON ExchangeInfo where
  parseJSON (Object !v) = liftM2 (\time -> ExchangeInfo . H.foldlWithKey' (process time) H.empty) (v .: "server_time") (v .: "pairs")
    where
      process :: Integer -> H.HashMap (Currency, Currency) PairInfo -> String -> PairInfo' -> H.HashMap (Currency, Currency) PairInfo
      process time r k v = let !time' = TOD time 0
                               (!from, !to) = second (toCurrency . tail) . first toCurrency . break (== '_') $ k
                           in  H.insert (to, from) (finishPairInfo time' to from k True  v)
                             . H.insert (from, to) (finishPairInfo time' from to k False v)
                             $ r
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

-- == Ticker == --

data Ticker = Ticker {-# UNPACK #-} !ClockTime
                     {-# UNPACK #-} !Rational
                     {-# UNPACK #-} !Rational
                     {-# UNPACK #-} !Rational
                     {-# UNPACK #-} !Rational
                     {-# UNPACK #-} !Rational
                     {-# UNPACK #-} !Rational
                     {-# UNPACK #-} !Rational
                     {-# UNPACK #-} !Rational

instance Timestamped Ticker where
  {-# SPECIALIZE instance Timestamped Ticker #-}
  timestamp   (Ticker x _ _ _ _ _ _ _ _) = x
bidPrice      (Ticker _ x _ _ _ _ _ _ _) = x
askPrice      (Ticker _ _ x _ _ _ _ _ _) = x
lastPrice     (Ticker _ _ _ x _ _ _ _ _) = x
highPrice     (Ticker _ _ _ _ x _ _ _ _) = x
averagePrice  (Ticker _ _ _ _ _ x _ _ _) = x
lowPrice      (Ticker _ _ _ _ _ _ x _ _) = x
volume        (Ticker _ _ _ _ _ _ _ x _) = x
currentVolume (Ticker _ _ _ _ _ _ _ _ x) = x

instance Show Ticker where
  showsPrec p x = showParen (p > 0)
                $ showString "Ticker {"
                . showString "timestamp = "       . showsPrec 0 (timestamp     x)
                . showString ", bidPrice = "      . showsPrec 0 (bidPrice      x)
                . showString ", askPrice = "      . showsPrec 0 (askPrice      x)
                . showString ", lastPrice = "     . showsPrec 0 (lastPrice     x)
                . showString ", highPrice = "     . showsPrec 0 (highPrice     x)
                . showString ", averagePrice = "  . showsPrec 0 (averagePrice  x)
                . showString ", lowPrice = "      . showsPrec 0 (lowPrice      x)
                . showString ", volume = "        . showsPrec 0 (volume        x)
                . showString ", currentVolume = " . showsPrec 0 (currentVolume x)
                . showChar   '}'

instance FromJSON Ticker where
  parseJSON (Object !v) = Ticker <$> liftM (flip TOD 0)   (v .: "updated")
                                 <*> v .: "buy"
                                 <*> v .: "sell"
                                 <*> v .: "last"
                                 <*> v .: "high"
                                 <*> v .: "avg"
                                 <*> v .: "low"
                                 <*> v .: "vol"
                                 <*> v .: "vol_cur"
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

-- == Offers == --

data Offer = Offer {
                     rate     :: {-# UNPACK #-} !Rational,
                     quantity :: {-# UNPACK #-} !Rational
                   }
  deriving (Show)

instance FromJSON Offer where
  parseJSON (Array a) = (\[a, b] -> Offer a b) <$> mapM parseJSON (V.toList a)
  parseJSON x          = fail ("Received wrong kind of JSON entity: " ++ show x)

data Offers = Offers {
                       asks :: ![Offer],
                       bids :: ![Offer]
                     }
  deriving (Show)

instance FromJSON Offers where
  parseJSON (Object !v) = liftM2 Offers (v .: "asks") (v .: "bids")
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)
 
-- == MyInfo == --

data MyInfo = MyInfo {-# UNPACK #-} !ClockTime
                                    !(H.HashMap Currency Rational)
                                    !Integer
                                    !Integer
                                    !Bool
                                    !Bool
                                    !Bool

instance Timestamped MyInfo where
  {-# SPECIALIZE instance Timestamped MyInfo #-}
  timestamp      (MyInfo x _ _ _ _ _ _) = x
instance FundsListing MyInfo where
  {-# SPECIALIZE instance FundsListing MyInfo #-}
  funds          (MyInfo _ x _ _ _ _ _) = x
transactionCount (MyInfo _ _ x _ _ _ _) = x
openOrderCount   (MyInfo _ _ _ x _ _ _) = x
canGetInfo       (MyInfo _ _ _ _ x _ _) = x
canTrade         (MyInfo _ _ _ _ _ x _) = x
canWithdraw      (MyInfo _ _ _ _ _ _ x) = x

instance Show MyInfo where
  showsPrec p x = showParen (p > 0)
                $ showString "MyInfo {"
                . showString "timestamp = "          . showsPrec 0 (timestamp        x)
                . showString ", funds = "            . showsPrec 0 (funds            x)
                . showString ", transactionCount = " . showsPrec 0 (transactionCount x)
                . showString ", openOrderCount = "   . showsPrec 0 (openOrderCount   x)
                . showString ", canGetInfo = "       . showsPrec 0 (canGetInfo       x)
                . showString ", canTrade = "         . showsPrec 0 (canTrade         x)
                . showString ", canWithdraw = "      . showsPrec 0 (canWithdraw      x)
                . showChar   '}'

instance FromJSON MyInfo where
  parseJSON (Object !v) = let rights = H.lookup "rights" $ v
                          in  case rights of
                                   Just (Object rights') -> MyInfo <$> liftM (flip TOD 0)                      (v .: "server_time")
                                                                   <*> liftM (H.foldlWithKey' convert H.empty) (v .: "funds")
                                                                   <*>                                         (v .: "transaction_count")
                                                                   <*>                                         (v .: "open_orders")
                                                                   <*> liftM (/= (0 :: Word8))           (rights' .: "info")
                                                                   <*> liftM (/= (0 :: Word8))           (rights' .: "trade")
                                                                   <*> liftM (/= (0 :: Word8))           (rights' .: "withdraw")
                                     where
                                       convert r k v = H.insert (toCurrency k) v r
                                   Nothing  -> fail "MyInfo is missing rights"
                                   _        -> fail "MyInfo has malformed rights"
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

-- == PlacedTrade == --

data PlacedTrade = PlacedTrade                !Integer
                               {-# UNPACK #-} !Rational
                               {-# UNPACK #-} !Rational
                                              !(H.HashMap Currency Rational)

orderID   (PlacedTrade x _ _ _) = x
received  (PlacedTrade _ x _ _) = x
remaining (PlacedTrade _ _ x _) = x
instance FundsListing PlacedTrade where
  {-# SPECIALIZE instance FundsListing PlacedTrade #-}
  funds   (PlacedTrade _ _ _ x) = x

instance Show PlacedTrade where
  showsPrec p x = showParen (p > 0)
                $ showString "PlacedTrade {"
                . showString "orderID = "     . showsPrec 0 (orderID   x)
                . showString ", received = "  . showsPrec 0 (received  x)
                . showString ", remaining = " . showsPrec 0 (remaining x)
                . showString ", funds = "     . showsPrec 0 (funds     x)
                . showChar   '}'

instance FromJSON PlacedTrade where
  parseJSON (Object !v) = PlacedTrade <$> v .: "order_id"
                                      <*> v .: "received"
                                      <*> v .: "remains"
                                      <*> liftM (H.foldlWithKey' convert H.empty) (v .: "funds")
    where
      convert r k v = H.insert (toCurrency k) v r
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

-- ==== PRIVATE UTILITY METHODS ==== --

intUpperBound :: Integer
intUpperBound = join seq $ toInteger (maxBound :: Int) - toInteger (minBound :: Int) + 1

rightMap :: (b -> c) -> Either a b -> Either a c
rightMap _ (Left  x) = Left x
rightMap f (Right x) = Right (f x)

showRational :: Rational -> String
showRational = uncurry f . properFraction
  where
    f n 0 = show n
    f n p = ((show n) ++) . ('.':) . concat . reverse . dropWhile (=="0") . reverse . take 8 . tail . map (show . fst) . iterate (properFraction . (*10) . snd) $ (0, p)

boundedLetterCountBreaks :: [(Int, Integer)]
letterCountBreaks :: [Integer]
(boundedLetterCountBreaks, letterCountBreaks) = let breaks = scanl (\r e -> r + 26^e) 0 [0..]
                                                in  (zip [0..] . takeWhile (<= intUpperBound) $ breaks, breaks)

convertInfo :: H.HashMap (Currency, Currency) PairInfo -> Gr Currency PairInfo
convertInfo info = let infoList   = H.toList $ info
                       nodeLabels = map head . group . sort . concatMap (\(a, b) -> [a, b]) . map fst $ infoList
                       nodeLookup = H.fromList . flip zip [0..] $ nodeLabels
                       nodes      = zip [0..] nodeLabels
                       edges      = map (\((from, to), edgeInfo) -> (H.lookupDefault 0 from nodeLookup, H.lookupDefault 0 to nodeLookup, edgeInfo)) $ infoList
                   in  mkGraph nodes edges

-- ==== CONFIGURATION ==== --

userAgent :: SB.ByteString
userAgent = "btce-hs/0.0 (GitHub Olathe/btce-hs)"

-- ==== LOW-LEVEL REQUEST HANDLING ==== --

makeRequest :: (FromJSON t) => SB.ByteString -> [(String, String)] -> Manager -> ResourceT IO (Either String t)
makeRequest !path !params !httpManager = do
                                             let params'       = SB.pack . ('?':) . urlEncodeVars $ params
                                                 pathAndParams = SB.concat [path, params']
                                             request' <- parseUrl ("https://btc-e.com/api/3/" ++ SB.unpack pathAndParams)
                                             let !request = request' {
                                                                       requestHeaders = ("User-Agent", userAgent):requestHeaders request',
                                                                       responseTimeout = Just 10000000,
                                                                       decompress = alwaysDecompress
                                                                     }
                                             response <- httpLbs request httpManager
                                             return . eitherDecode . responseBody $ response

newtype SecureResponse t = SecureResponse t
  deriving (Show)

unwrapSecureResponse :: Either String (SecureResponse t) -> Either String t
unwrapSecureResponse (Left x)                    = Left  x
unwrapSecureResponse (Right (SecureResponse !x)) = Right x

instance (FromJSON t) => FromJSON (SecureResponse t) where
  parseJSON (Object !v) = case H.lookup "success" v of
                               Just (Number 0) -> case H.lookup "error" v of
                                    Just (String !x) -> fail . T.unpack $ x
                                    Just _           -> fail "Not a SecureResponse (error message is not a string)"
                                    Nothing          -> fail "Not a SecureResponse (missing error message)"
                               Just (Number _) -> case H.lookup "return" v of
                                    Just !o -> fmap SecureResponse . parseJSON $ o
                                    Nothing -> fail "Not a SecureResponse (missing return value)"
                               Just _          -> fail "Not a SecureResponse (success indication is not a number)"
                               Nothing         -> fail "Not a SecureResponse (missing success indication)"
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

-- Note: The nonces must be received by the server in numeric order.
--       This introduces possible race conditions if the program using this library is multithreaded.
--       Avoid races by holding the MVar until we receive either a successful response or an unrecoverable error response.
makeSecureRequest :: (FromJSON t) => String -> [(String, String)] -> Account -> Manager -> ResourceT IO (Either String t)
makeSecureRequest !method !params !(Account !key !secret !nonceRef) !httpManager = let !params' = map (join (***) SB.pack) . (("method", method):) $ params
                                                                                   in  lift (takeMVar nonceRef) >>= go params' key secret nonceRef httpManager
  where
    go :: (FromJSON t) => [(SB.ByteString, SB.ByteString)] -> SB.ByteString -> LB.ByteString -> MVar Word32 -> Manager -> Word32 -> ResourceT IO (Either String t)
    go params key secret nonceRef httpManager !nonce = do
      when (nonce == maxBound) $ lift (putMVar nonceRef nonce >> throw APIKeyUsedUp)
      request' <- parseUrl "https://btc-e.com/tapi"
      let !request = signedURLEncodedBody secret nonce params $
                      request' { 
                                 requestHeaders = ("User-Agent", userAgent):("Key", key):requestHeaders request',
                                 responseTimeout = Just 10000000,
                                 decompress = alwaysDecompress
                               }
      !res <- fmap responseBody $ httpLbs request httpManager
      let !response = unwrapSecureResponse . eitherDecode $ res
      case response of
        Left r -> case stripPrefix "invalid nonce parameter; on key:" r of
          Just ys -> go params key secret nonceRef httpManager . succ . read . takeWhile isDigit $ ys
          Nothing -> lift (putMVar nonceRef (succ nonce)) >> return (if r == "Failed reading: satisfy" then Left (LB.unpack res) else response)
        _      -> lift (putMVar nonceRef (succ nonce)) >> return response
    
    addNonceParameter :: Word32 -> [(SB.ByteString, SB.ByteString)] -> [(SB.ByteString, SB.ByteString)]
    addNonceParameter !nonce = (("nonce", SB.pack (show nonce)):) . filter ((/= "nonce") . map toLower . SB.unpack . fst)
    
    signedURLEncodedBody :: LB.ByteString -> Word32 -> [(SB.ByteString, SB.ByteString)] -> Request -> Request
    signedURLEncodedBody !secret !nonce !params !request = let request'            = urlEncodedBody (addNonceParameter nonce params) request
                                                               RequestBodyLBS body = requestBody request'
                                                               signature           = SB.pack . showDigest . hmacSha512 secret $ body
                                                           in  request' { requestHeaders = (("Sign", signature):) . filter ((/= "Sign") . fst) . requestHeaders $ request' }

-- For debugging
makeSecureRequest' :: String -> [(String, String)] -> Account -> Manager -> ResourceT IO String
makeSecureRequest' !method !params !(Account !key !secret !nonceRef) !httpManager = let !params' = map (join (***) SB.pack) . (("method", method):) $ params
                                                                            in  lift (takeMVar nonceRef) >>= go params' key secret nonceRef httpManager
  where
    go :: [(SB.ByteString, SB.ByteString)] -> SB.ByteString -> LB.ByteString -> MVar Word32 -> Manager -> Word32 -> ResourceT IO String
    go params key secret nonceRef httpManager !nonce = do
      lift $ print nonce
      when (nonce == maxBound) $ lift (putMVar nonceRef nonce >> throw APIKeyUsedUp)
      request' <- parseUrl "https://btc-e.com/tapi"
      let !request = signedURLEncodedBody secret nonce params $
                      request' { 
                                 requestHeaders = ("User-Agent", userAgent):("Key", key):requestHeaders request',
                                 responseTimeout = Just 10000000,
                                 decompress = alwaysDecompress
                               }
      !response <- httpLbs request httpManager
      let !text = LB.unpack $ responseBody response
      lift $ print text
      case removeStart "{\"success\":0,\"error\":\"invalid nonce parameter; on key:" text of
        Just xs -> go params key secret nonceRef httpManager . succ . read . takeWhile isDigit $ xs
        Nothing -> lift (putMVar nonceRef (succ nonce)) >> return text
    
    addNonceParameter :: Word32 -> [(SB.ByteString, SB.ByteString)] -> [(SB.ByteString, SB.ByteString)]
    addNonceParameter !nonce = (("nonce", SB.pack (show nonce)):) . filter ((/= "nonce") . map toLower . SB.unpack . fst)
    
    signedURLEncodedBody :: LB.ByteString -> Word32 -> [(SB.ByteString, SB.ByteString)] -> Request -> Request
    signedURLEncodedBody !secret !nonce !params !request = let request'            = urlEncodedBody (addNonceParameter nonce params) request
                                                               RequestBodyLBS body = requestBody request'
                                                               signature           = SB.pack . showDigest . hmacSha512 secret $ body
                                                           in  request' { requestHeaders = (("Sign", signature):) . filter ((/= "Sign") . fst) . requestHeaders $ request' }
    
    removeStart :: (Eq a) => [a] -> [a] -> Maybe [a]
    removeStart []     !xs    = Just xs
    removeStart _      []     = Nothing
    removeStart (x:xs) (y:ys) = join seq $ if x == y then removeStart xs ys else Nothing
