{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where
import Data.Maybe
import Text.HTML.Scalpel

numProfessors :: String -> IO (Maybe Int)
numProfessors [] = return Nothing 
numProfessors department = do 
   res0 <- scrapeURL "https://www.gla.ac.uk/schools/chemistry/staff/" scrapeComic
   return res0

scrapeComic :: Scraper String Int
scrapeComic = 
  chroot ("ul" @: ["id" @= "research-teachinglist"]) scrapeTitle

scrapeTitle :: Scraper String Int
scrapeTitle = do
   return 4
