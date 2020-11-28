{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where
import Data.Maybe
import Data.List
import Text.HTML.Scalpel

numProfessors :: String -> IO (Maybe [String])
numProfessors [] = return Nothing 
numProfessors department = do 
   res0 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeUL "research-teaching")
   res1 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeUL "professional-administrative-support")
   res2 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeUL "affiliate")
   
   return res0

scrapeUL :: String -> Scraper String [String]
scrapeUL school = 
   chroot ("div" @: ["id" @=school]) scrapeProfessors

scrapeProfessors :: Scraper String [String]
scrapeProfessors = do
   strs <- texts "a"
   return strs
   

        
--isProfessor :: String -> Bool 
--isProfessor str =  isInfixOf "Professor" str 
