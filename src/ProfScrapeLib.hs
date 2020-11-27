{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where
import Text.HTML.Scalpel
import Data.List
numProfessors :: String -> IO (Maybe Int)
numProfessors [] = return Nothing
numProfessors department = do
   res0 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeSchool "research-teaching")
   res1 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeSchool "professional-administrative-support")
   res2 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeSchool "affiliate")
   sumResult res0 res1 res2 
      
sumResult:: Int->Int->Int->Int
sumResult a b c = a+b+c


scrapeSchool :: String -> Scraper String String
scrapeSchool school = chroot ("div" @: ["id" @=school]) scraperATags

scraperATags :: String -> Int 
scraperATags = do
   str <- texts "a"
   sum map countProfessors str 
  
countProfessors :: String -> Int
countProfessors str = length (filter (\x -> x == "Professor") str)
