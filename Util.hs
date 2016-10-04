{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Util (
	mconcat, (.), (++), Text, replace, xml, spanTag, h,
	anchor, Anchor(..), writeFile, greekAlphabet
	) where

import Prelude hiding ((.), (++), writeFile)
import qualified Data.Text as Text
import Data.Text (Text, replace)
import Data.Text.IO (writeFile)

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

greekAlphabet :: [(String, String)]
greekAlphabet =
	[ ("alpha"          , "&#945;")
	, ("beta"           , "&#946;")
	, ("delta"          , "&#948;")
	, ("mu"             , "&#956;")
	, ("nu"             , "&#957;")
	, ("lambda"         , "&#955;")
	, ("pi"             , "&#960;")
	, ("phi"            , "&#966;")
	, ("rho"            , "&#961;")
	, ("sigma"          , "&#963;")
	, ("theta"          , "&#952;")
	, ("zeta"           , "&#950;")

	, ("Gamma"          , "&#915;")
	, ("Pi"             , "&#928;") ]
