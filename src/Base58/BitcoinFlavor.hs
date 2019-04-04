{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Base58.BitcoinFlavor (encode, decode) where

import Data.List
import Data.Bits

import qualified Data.ByteString as BS

digits :: String
digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

encode :: BS.ByteString -> String
encode bs = let
	(zs, bs') = BS.span (== 0) bs in
	replicate (BS.length zs) '1' ++ encodeInteger (bsToInteger bs')
	
decode :: String -> Maybe BS.ByteString
decode str = do
	let	(zs, str') = span (== '1') str
	bs <- decodeInteger str'
	return $ BS.replicate (length zs) 0 <> integerToBs bs

bsToInteger :: BS.ByteString -> Integer
bsToInteger = toI . reverse . BS.unpack
	where
	toI [] = 0
	toI (w : ws) = fromIntegral w .|. toI ws `shiftL` 8

integerToBs :: Integer -> BS.ByteString
integerToBs = BS.pack . reverse . fromI
	where
	fromI 0 = []
	fromI n = fromIntegral n : fromI (n `shiftR` 8)
	
encodeInteger :: Integer -> String
encodeInteger = reverse . enc
	where
	enc n | n < 1 = ""
	enc n = digits !! (fromIntegral $ n `mod` 58) : enc (n `div` 58)

decodeInteger :: String -> Maybe Integer
decodeInteger = dec . reverse
	where
	dec "" = Just 0
	dec (d : ds) = do
		m <- d `elemIndex` digits
		n <- dec ds
		return $ n * 58 + fromIntegral m
