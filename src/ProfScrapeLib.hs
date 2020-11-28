{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where
import Data.Maybe
import Text.HTML.Scalpel

numProfessors :: String -> IO (Maybe String)
numProfessors [] = return Nothing 
numProfessors department = do 
   res0 <- scrapeURL "https://www.gla.ac.uk/schools/chemistry/staff/" scrapeComic
   return res0  

scrapeComic :: Scraper String String
scrapeComic = 
  chroot ("ul" @: ["id" @= "research-teachinglist"]) scrapeTitle

scrapeTitle :: Scraper String String
scrapeTitle = text "a"
