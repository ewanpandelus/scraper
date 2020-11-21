{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where

numProfessors :: String -> IO (Maybe Int)
numProfessors x = return Nothing
