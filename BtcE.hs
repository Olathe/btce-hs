{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings, BangPatterns #-}

-- BTC-E.com API

module BtcE where

import qualified Data.ListLike              as LL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8      as SB
import qualified Data.Vector                as V
import qualified Data.Text                  as T
import qualified Data.HashMap.Strict        as H
import qualified Data.Graph.Inductive.Graph as G

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
                                     def, secure, host, port, path, responseTimeout, decompress, alwaysDecompress)
import Data.Aeson                   (FromJSON, Value(Object, Array, String, Number), (.:), parseJSON, eitherDecode)
import Data.Aeson.Types             (Parser)

import Data.Typeable      (Typeable)
import Data.Hashable      (Hashable)
import GHC.Generics       (Generic)
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

-- == Currency == --

data Currency = BTC | LTC | FTC | NMC | NVC | PPC | TRC | USD | EUR | RUR | NeedsToBeAdded !String
  deriving (Generic, Show, Eq, Ord)

instance Hashable Currency

instance Enum Currency where
  fromEnum BTC = 0
  fromEnum LTC = 1
  fromEnum FTC = 2
  fromEnum NMC = 3
  fromEnum NVC = 4
  fromEnum PPC = 5
  fromEnum TRC = 6
  fromEnum USD = 7
  fromEnum EUR = 8
  fromEnum RUR = 9
  fromEnum (NeedsToBeAdded xs) = join seq . fromInteger . uncurry (+) . first head . foldl' (\((_:bs), !r) e -> (bs, 26*r + e)) (letterCountBreaks, 0) . map (toInteger . subtract (ord 'a') . ord) $ xs
  
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
fromCurrency BTC = "btc"
fromCurrency LTC = "ltc"
fromCurrency FTC = "ftc"
fromCurrency NMC = "nmc"
fromCurrency NVC = "nvc"
fromCurrency PPC = "ppc"
fromCurrency TRC = "trc"
fromCurrency USD = "usd"
fromCurrency EUR = "eur"
fromCurrency RUR = "rur"
fromCurrency (NeedsToBeAdded x) = x

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
getAccount key secret = lift (newMVar 1) >>= return . Account (LL.fromString (LL.toString key)) (LL.fromString (LL.toString secret))

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
                                             <*> liftM2 (\x y -> Offer (repairRational x) (repairRational y)) (v .: "price") (v .: "amount")
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
  parseJSON (Object !v) = PairInfo' <$> liftM repairRational            (v .: "min_price")
                                    <*> liftM repairRational            (v .: "max_price")
                                    <*> liftM repairRational            (v .: "min_amount")
                                    <*> liftM ((/100) . repairRational) (v .: "fee")
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
                                 <*> liftM repairRational (v .: "buy")
                                 <*> liftM repairRational (v .: "sell")
                                 <*> liftM repairRational (v .: "last")
                                 <*> liftM repairRational (v .: "high")
                                 <*> liftM repairRational (v .: "avg")
                                 <*> liftM repairRational (v .: "low")
                                 <*> liftM repairRational (v .: "vol")
                                 <*> liftM repairRational (v .: "vol_cur")
  parseJSON x           = fail ("Received wrong kind of JSON entity: " ++ show x)

-- == Offers == --

data Offer = Offer {
                     rate     :: {-# UNPACK #-} !Rational,
                     quantity :: {-# UNPACK #-} !Rational
                   }
  deriving (Show)

instance FromJSON Offer where
  parseJSON (Array a) = (\[a, b] -> Offer a b) <$> mapM (liftM repairRational . parseJSON) (V.toList a)
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
                                       convert r k v = H.insert (toCurrency k) (repairRational v) r
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
  parseJSON (Object !v) = PlacedTrade <$>                                         (v .: "order_id")
                                      <*> liftM (repairRational)                  (v .: "received")
                                      <*> liftM (repairRational)                  (v .: "remains")
                                      <*> liftM (H.foldlWithKey' convert H.empty) (v .: "funds")
    where
      convert r k v = H.insert (toCurrency k) (repairRational v) r
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

repairRational :: Rational -> Rational
repairRational = (/(10^8)) . toRational . round . ((10^8)*)

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
makeRequest !path !params !httpManager = let params'  = SB.pack . ('?':) . urlEncodeVars $ params
                                             !request = def {
                                                              secure = True,
                                                              host = "btc-e.com",
                                                              port = 443,
                                                              path = SB.concat ["/api/3/", path, params'],
                                                              requestHeaders = ("User-Agent", userAgent):requestHeaders def,
                                                              responseTimeout = Just 10000000,
                                                              decompress = alwaysDecompress
                                                            }
                                         in liftM (eitherDecode . responseBody) $ httpLbs request httpManager

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
      let !request = signedURLEncodedBody secret nonce params $
                      def { 
                            secure = True,
                            host = "btc-e.com",
                            port = 443,
                            path = "/tapi",
                            requestHeaders = ("User-Agent", userAgent):("Key", key):requestHeaders def,
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
    
    signedURLEncodedBody :: (Monad m) => LB.ByteString -> Word32 -> [(SB.ByteString, SB.ByteString)] -> Request m -> Request m
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
      let !request = signedURLEncodedBody secret nonce params $
                      def { 
                            secure = True,
                            host = "btc-e.com",
                            port = 443,
                            path = "/tapi",
                            requestHeaders = ("User-Agent", userAgent):("Key", key):requestHeaders def,
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
    
    signedURLEncodedBody :: (Monad m) => LB.ByteString -> Word32 -> [(SB.ByteString, SB.ByteString)] -> Request m -> Request m
    signedURLEncodedBody !secret !nonce !params !request = let request'            = urlEncodedBody (addNonceParameter nonce params) request
                                                               RequestBodyLBS body = requestBody request'
                                                               signature           = SB.pack . showDigest . hmacSha512 secret $ body
                                                           in  request' { requestHeaders = (("Sign", signature):) . filter ((/= "Sign") . fst) . requestHeaders $ request' }
    
    removeStart :: (Eq a) => [a] -> [a] -> Maybe [a]
    removeStart []     !xs    = Just xs
    removeStart _      []     = Nothing
    removeStart (x:xs) (y:ys) = join seq $ if x == y then removeStart xs ys else Nothing
