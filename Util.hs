{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Util (
	mconcat, (.), (++), Text, replace, xml, spanTag, h,
	anchor, Anchor(..), writeFile, greekAlphabet
	) where

import Prelude hiding ((.), (++), writeFile)
import qualified Data.Text as Text
import Data.Text (Text, replace, unpack)
import qualified System.Info as SysInfo
import qualified Data.Char as Char
import Data.Bits ((.&.), shiftR)
import qualified Data.Text.IO as TextIO
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

(++) :: Monoid a => a -> a -> a
(++) = mappend

xml :: Text -> [(Text, Text)] -> Text -> Text
xml t attrs = (("<" ++ t ++ " " ++ Text.unwords (map f attrs) ++ ">") ++) . (++ ("</" ++ t ++ ">"))
	where
		f (n, v) = n ++ "='" ++ v ++ "'"

spanTag :: Text -> Text -> Text
spanTag = xml "span" . (:[]) . ("class",)

h :: Int -> Text -> Text
h = flip xml [] . ("h" ++) . Text.pack . show

data Anchor = Anchor { aClass, aId, aHref, aText, aStyle :: Text }

anchor :: Anchor
anchor = Anchor{aClass="", aId="", aHref="", aText="", aStyle=""}

decomposeUtf8 :: Text -> ByteString
decomposeUtf8 t = ByteString.pack $ map (Char.chr) $ decInt $ map (Char.ord) $ unpack t
	where
		decInt :: [Int] -> [Int]
		decInt [] = []
		decInt (c : cs)
			| c < 0x80     = c : decInt cs -- 7 bits
			| c < 0x800    = (u8 0xc0 c 6) : (u8 0x80 c 0) : decInt cs -- 5+6 bits
			| c < 0x10000  = (u8 0xe0 c 12) : (u8 0x80 c 6) : (u8 0x80 c 0) : decInt cs -- 4+6+6 bits
			| c < 0x200000 = (u8 0xf0 c 18) : (u8 0x80 c 12) : (u8 0x80 c 6) : (u8 0x80 c 0) : decInt cs -- 3+6+6+6 bits
			| otherwise    = 0xef : 0xbf : 0xbd : decInt cs -- U+FFFD
		
		u8 :: Int -> Int -> Int -> Int
		u8 prefix value shift = prefix + (0x3f .&. shiftR value shift)

writeFile :: FilePath -> Text -> IO ()
writeFile path content = case SysInfo.os of
	"mingw32" -> ByteString.writeFile path (decomposeUtf8 content)
	_ -> TextIO.writeFile path content

greekAlphabet :: [(String, Char)]
greekAlphabet =
	[ ("alpha"          , 'α')
	, ("beta"           , 'β')
	, ("delta"          , 'δ')
	, ("mu"             , 'μ')
	, ("nu"             , 'ν')
	, ("lambda"         , 'λ')
	, ("pi"             , 'π')
	, ("phi"            , 'φ')
	, ("rho"            , 'ρ')
	, ("sigma"          , 'σ')
	, ("theta"          , 'θ')
	, ("zeta"           , 'ζ')

	, ("Gamma"          , 'Γ')
	, ("Pi"             , 'Π') ]
