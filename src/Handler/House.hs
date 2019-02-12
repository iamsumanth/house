{-# LANGUAGE OverloadedStrings #-}
module Handler.House where

import Import
import Model.House
import Model.Person
import Model.Address

getHouseR :: Handler Value
getHouseR = return $ toJSON getHouses

postHouseR :: Handler TypedContent
postHouseR = sendResponseNoContent

getHouses :: [House]
getHouses = [House (Address "12/B" "Gachibowli" "500032") (Person "Shashi" "12345") 10000]
