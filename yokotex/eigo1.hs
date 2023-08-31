{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Random (randomRIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

data Be = Be | Am | Are | Is deriving Eq
data WClass = S | V | C deriving Eq
data Wtype =  To | Fr | On | Th | Fo | Gr | Pl | Pa | PaO | CS | CD | CR | CT deriving Eq
-- To, From, On, Thing, Food, Greeting, Place, Parson, Parson(Objective), CanSit, CanDraw
-- CanRead, CanTell
data Jtype = Wo | Ni | Kr | T | P Int deriving Eq
-- を に から と (位置 P)

type Subject = T.Text 
type JpSubject = T.Text 
type Verb =  T.Text
type JpVerb = T.Text
type Noun = T.Text
type JpNoun = T.Text
type Athe = T.Text 
type Qtype = (Bool,Bool,Bool,Bool) -- subject, verbNow, verbPast, verbIng

makeVerbPart :: [T.Text] -> [((JpVerb,(Verb,[Wtype],[Jtype],JpVerb,JpVerb)),((Verb,Verb),(Verb,Verb),(Verb,Verb)))] 
makeVerbPart [] = []
makeVerbPart (v:vs) =
  let (everb:everbN:everbP:everbF:everbI:jverb:jverbP:jverbI:verbteL:verbtjL:xs) = T.splitOn " " v
   in ((jverb,(everb,verbteL,verbtjL,jverbP,jverbI)),((everb,everbN),(everb,everbP),(everb,everbI))):makeVerbPart vs
--}


