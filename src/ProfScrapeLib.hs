{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where
import Data.Maybe
import Data.List
import Text.HTML.Scalpel
import Control.Applicative

numProfessors :: String -> IO (Maybe Int)
numProfessors [] = return Nothing 
numProfessors department = do 
   res0 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeUL "research-teaching")
   res1 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeUL "professional-administrative-support")
   res2 <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ department ++ "/staff") (scrapeUL "affiliate")
   let resList= [res0,res1,res2]  
   return $ sumResult resList
 
sumResult::[Maybe Int]->Maybe Int
sumResult x = foldl (\x y  -> pure (+) <*> x <*> y) (Just 0) x


scrapeUL :: String -> Scraper String Int
scrapeUL school = 
   chroot ("div" @: ["id" @=school]) scrapeProfessors

scrapeProfessors :: Scraper String Int
scrapeProfessors = do
   strs <- texts "a"
   return(sum $ map countProfs strs)
  
countProfs::String -> Int
countProfs[] = 0
countProfs strs = length (filter (\x -> isInfixOf x "Professor") $ words strs)

        
--isProfessor :: String -> Bool 
--isProfessor str =  isInfixOf "Professor" str 
