{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where
import Data.Maybe
import Text.HTML.Scalpel

numProfessors :: String -> IO (Maybe String)
numProfessors [] = return Nothing 
numProfessors department = do 
   res0 <- scrapeURL "http://xkcd.com" scrapeComic
   return res0  

scrapeComic :: Scraper String String
scrapeComic = 
  chroot ("div" @: ["id" @= "comic"]) scrapeTitle

scrapeTitle :: Scraper String String
scrapeTitle = (attr "title" "img")
